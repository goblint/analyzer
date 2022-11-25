#!/usr/bin/env ruby

# gobopt environment variable can be used to override goblint defaults and PARAMs

require 'find'
require 'fileutils'
require 'timeout'
require 'pathname'
def relpath(file)
  return Pathname(file).relative_path_from Pathname(Dir.getwd) # Pathname for arg required for ruby 2.5, 2.6 accepts string as well
end
require 'set'
$timeout = 20 # seconds

def puts(o) # puts is not atomic and messes up linebreaks with multiple threads
  print(o+"\n")
end
# colors
class String
  def indent(n=2); " "*n + self end
  # colors
  def colorize(color_code); "\e[#{color_code}m#{self}\e[0m" end
  def black; colorize(30) end
  def red; colorize(31) end
  def green; colorize(32) end
  def yellow; colorize(33) end
  def blue; colorize(34) end
  def pink; colorize(35) end
  def cyan; colorize(36) end
  def white; colorize(37) end
  def bg_black; colorize(40) end # gray for me
  def gray; colorize("38;5;240") end
end
class Array
  def itemize(n=2); self.map {|x| "- #{x}".indent(n)}.join() end
end
# clear the current line
def clearline
  print "\r\e[K"
end

$goblint = File.join(Dir.getwd,"goblint")
goblintbyte = File.join(Dir.getwd,"goblint.byte")
if File.exists?(goblintbyte) then
  puts "Running the byte-code version! Continue? (y/n)"
  exit unless $stdin.gets()[0] == 'y'
  $goblint = goblintbyte
elsif not File.exist?($goblint) then
  fail "Goblint not present in working directory. Please run script from goblint dir!"
end
$vrsn = `#{$goblint} --version`

if not File.exists? "linux-headers" then
  puts "Missing linux-headers, will download now!"
  `make headers`
end
has_linux_headers = File.exists? "linux-headers" # skip kernel tests if make headers failed (e.g. on opam-repository opam-ci where network is forbidden)

#Command line parameters
#Either only run a single test, or
#"future" will also run tests we normally skip
$dump = ARGV.last == "-d" && ARGV.pop
sequential = ARGV.last == "-s" && ARGV.pop
marshal = ARGV.last == "-m" && ARGV.pop
incremental = ARGV.last == "-i" && ARGV.pop
report = ARGV.last == "-r" && ARGV.pop
only = ARGV[0] unless ARGV[0].nil?
if marshal || incremental then
  sequential = true
end
if marshal && incremental then
  fail "Marshal (-m) and Incremental (-i) tests can not be activated at the same time!"
end
if only == "future" then
  future = true
  only = nil
elsif only == "group" then
  thegroup = ARGV[1]
  future = thegroup.start_with?"-"
  future = !future # why does negation above fail?
  only = nil
else
  future = false
end

$testresults = File.expand_path("tests/suite_result")
testfiles    = if incremental then
                 File.expand_path("tests/incremental")
               else
                 File.expand_path("tests/regression")
               end

$alliswell = true
$failed    = [] # failed tests
$timedout  = [] # timed out tests

$highlighter = lambda {|f,o| "cp #{f} #{o}"}
if report then
  cmds = {"code2html" => lambda {|f,o| "code2html -l c -n #{f} 2> /dev/null 1> #{o}"},
          "source-highlight" => lambda {|f,o| "source-highlight -n -i #{f} -o #{o}"},
          "pygmentize" => lambda {|f,o| "pygmentize -O full,linenos=1 -o #{o} #{f}"}
        }
  cmds.each do |name, cmd|
    # if `which #{cmd} 2> /dev/null`.empty? then
    if ENV['PATH'].split(':').map {|f| File.executable? "#{f}/#{name}"}.include?(true) then
      $highlighter = cmd
      break
    end
  end
  if $highlighter.nil? then
    puts "Warning: No syntax highlighter installed (code2html, source-highlight, pygmentize)."
  end
end

class Tests
  attr_reader :tests, :tests_line, :todo
  attr_accessor :p, :warnfile, :statsfile, :orgfile, :cilfile, :ok, :correct, :ignored, :ferr, :warnings, :vars, :evals
  def initialize(project, tests, tests_line, todo)
    @p = project
    @tests = tests
    @tests_line = tests_line
    @todo = todo
    @ok = true
    @correct = 0
    @ignored = 0
    @ferr = nil
    @vars = 0
    @evals = 0
    @warnings = Hash.new
  end

  def report
    filename = File.basename(p.path)
    system($highlighter.call(filename, orgfile))
    `#{$goblint} #{filename} --set justcil true #{p.params} >#{cilfile} 2> /dev/null`
    p.size = `wc -l #{cilfile}`.split[0]
  end

  def collect_warnings
    warnings[-1] = "term"
    lines = IO.readlines(warnfile, :encoding => "UTF-8")
    lines.each do |l|
      if l =~ /Function 'main' does not return/ then warnings[-1] = "noterm" end
      if l =~ /vars = (\d*).*evals = (\d+)/ then
        @vars = $1
        @evals = $2
      end
      next unless l =~ /(.*)\(.*?\:(\d+)(?:\:\d+)?(?:-(?:\d+)(?:\:\d+)?)?\)/
      obj,i = $1,$2.to_i

      ranking = ["other", "warn", "race", "norace", "deadlock", "nodeadlock", "success", "fail", "unknown", "term", "noterm"]
      thiswarn =  case obj
                    when /\(conf\. \d+\)/            then "race"
                    when /Deadlock/                  then "deadlock"
                    when /lock (before|after):/      then "deadlock"
                    when /Assertion .* will fail/    then "fail"
                    when /Assertion .* will succeed/ then "success"
                    when /Assertion .* is unknown/   then "unknown"
                    when /invariant confirmed/       then "success"
                    when /invariant unconfirmed/     then "unknown"
                    when /invariant refuted/         then "fail"
                    when /^\[Warning\]/              then "warn"
                    when /^\[Error\]/                then "warn"
                    when /^\[Info\]/                 then "warn"
                    when /^\[Success\]/              then "success"
                    when /\[Debug\]/                 then next # debug "warnings" shouldn't count as other warnings (against NOWARN)
                    when /^  on line \d+ $/          then next # dead line warnings shouldn't count (used for unreachability with NOWARN)
                    when /^  on lines \d+..\d+ $/    then next # dead line warnings shouldn't count (used for unreachability with NOWARN)
                    else "other"
                  end
      oldwarn = warnings[i]
      if oldwarn.nil? then
        warnings[i] = thiswarn
      else
        warnings[i] = ranking[[ranking.index(thiswarn), ranking.index(oldwarn)].max]
      end
    end
  end

  def compare_warnings
    tests.each_pair do |idx, type|
      check = lambda {|cond|
        if cond then
          @correct += 1
          # full p.path is too long and p.name does not allow click to open in terminal
          if todo.include? idx then puts "Excellent: ignored check on #{relpath(p.path).to_s.cyan}:#{idx.to_s.blue} is now passing!" end
        else
          if todo.include? idx then @ignored += 1 else
            puts "Expected #{type.yellow}, but registered #{(warnings[idx] or "nothing").yellow} on #{p.name.cyan}:#{idx.to_s.blue}"
            puts tests_line[idx].rstrip.gray
            ferr = idx if ferr.nil? or idx < ferr
          end
        end
      }
      case type
      when "deadlock", "race", "fail", "noterm", "unknown", "term", "warn"
        check.call warnings[idx] == type
      when "nowarn"
        check.call warnings[idx].nil?
      when "assert", "success"
        check.call warnings[idx] == "success"
      when "norace"
        check.call warnings[idx] != "race"
      when "nodeadlock"
        check.call warnings[idx] != "deadlock"
      end
    end
  end

  def time_to_html
    lines = IO.readlines(statsfile, :encoding => "UTF-8")
    res = lines.grep(/^TOTAL\s*(.*) s.*$/) { $1 }
    errors = lines.grep(/Error:/)
    if res == [] or not errors == [] then
      ok = false
      "<td><a href=\"#{statsfile}\">failure</a></td>"
    else
      "<td><a href=\"#{statsfile}\">#{"%.3f" % res} s</a></td>"
    end
  end

  def problems_to_html
    id = "#{p.id} #{p.group}/#{p.name}"
    lines = IO.readlines(statsfile, :encoding => "UTF-8")
    if correct + ignored == tests.size && ok then
      "<td style =\"color: green\">NONE</td>"
    else
      $alliswell = false
      if not $timedout.include? id then
        $failed.push "#{p.id} #{p.name}"
        exc = if lines[0] =~ /exception/ then " (see exception above)" else "" end
        puts "#{p.id}" + " failed#{exc}!".red
        puts ""
        if $dump then
          puts "============== WARNINGS ==============="
          puts File.read(warnfile)
          puts "================ STATS ================"
          puts File.read(statsfile)
          puts "======================================="
        end
      end
      if not ok or ferr.nil? then
        "<td style =\"color: red\">FAILED</td>"
      else
        whataglorifiedmess = File.join(p.group, p.name + ".c.html")
        "<td><a href=\"#{whataglorifiedmess}#line#{ferr}\" style =\"color: red\">LINE #{ferr}</a></td>"
      end
    end
  end

  def to_html
    "<td><a href=\"#{orgfile}\">#{p.name}</a></td>\n" +
    "<td><a href=\"#{cilfile}\">#{p.size} lines</a></td>\n" +
    "<td><a href=\"#{warnfile}\">#{correct} of #{tests.size}</a></td>" +
    time_to_html +
    "<td>#{vars} / #{evals}</a></td>" +
    problems_to_html
  end
end

class Project
  attr_reader :id, :name, :group, :path, :params, :testset, :html_heading
  attr_accessor :size, :testset
  def initialize(id, name, group, path, params)
    @id       = id
    @name     = name
    @size     = 0
    @group    = group
    @path     = path
    @params   = params
    @html_heading = ["ID", "Name", "Size (CIL)", "Checks", "Time", "Vars / Eval", "Problems"]
  end

  def parse_tests (lines)
    tests = Hash.new
    todo = Set.new
    tests_line = Hash.new
    i = 0
    lines.each do |obj|
      i = i + 1
      if obj =~ /#line ([0-9]+).*$/ then
        i = $1.to_i - 1
      end
      next if obj =~ /^\s*\/\// || obj =~ /^\s*\/\*([^*]|\*+[^*\/])*\*\/$/
      todo << i if obj =~ /TODO|SKIP/
      tests_line[i] = obj
      if obj =~ /RACE/ then
        tests[i] = if obj =~ /NORACE/ then "norace" else "race" end
      elsif obj =~ /DEADLOCK/ then
        tests[i] = if obj =~ /NODEADLOCK/ then "nodeadlock" else "deadlock" end
      elsif obj =~ /WARN/ then
        tests[i] = if obj =~ /NOWARN/ then "nowarn" else "warn" end
      elsif obj =~ /SUCCESS/ then
        tests[i] = "success"
      elsif obj =~ /FAIL/ then
        tests[i] = "fail"
      elsif obj =~ /UNKNOWN/ then
        tests[i] = "unknown"
      elsif obj =~ /(assert|__goblint_check).*\(/ then
        if obj =~ /FAIL/ then
          tests[i] = "fail"
        elsif obj =~ /UNKNOWN/ then
          tests[i] = "unknown"
        else
          tests[i] = "assert"
        end
      end
    end
    case lines[0]
    when /NON?TERM/
      tests[-1] = "noterm"
    when /TERM/
      tests[-1] = "term"
    end
    Tests.new(self, tests, tests_line, todo)
  end

  def create_test_set(lines)
    @testset = parse_tests(lines)
    @testset.warnfile = File.join($testresults, group, name + ".warn.txt")
    @testset.statsfile = File.join($testresults, group, name + ".stats.txt")
    @testset.orgfile = File.join($testresults, group, name + ".c.html")
    @testset.cilfile = File.join($testresults, group, name + ".cil.txt")
  end

  def run_testset (testset, cmd, starttime)
    strid = "#{id} #{group}/#{name}"
    pid = Process.spawn(cmd, :pgroup=>true)
    begin
      Timeout::timeout($timeout) {Process.wait pid}
    rescue Timeout::Error
      pgid = Process.getpgid(pid)
      puts "\t #{strid} reached timeout of #{$timeout}s!".red + " Killing pgid #{pgid}..."
      $timedout.push strid
      Process.kill('KILL', -1*pgid)
      testset.ok = false
      return self
    end
    endtime   = Time.now
    status = $?.exitstatus
    if status != 0 then
      reason = if status == 1 then "error" elsif status == 2 then "exception" elsif status == 3 then "verify" end
      clearline
      puts "Testing #{strid}" + "\t Status: #{status} (#{reason})".red
      stats = File.readlines testset.statsfile
      if status == 1 then
        puts stats.last(5).itemize
      elsif status == 2 then # if stats[0] =~ /exception/ then
        lastline = (File.readlines testset.warnfile).last()
        filename = File.basename(@path)
        puts lastline.strip().sub filename, relpath(@path).to_s unless lastline.nil?
        puts stats[0..9].itemize
      elsif status == 3 then
        warn = File.readlines testset.warnfile
        puts (warn.select { |x| x["Unsatisfied constraint"] || x["Fixpoint not reached"] }).uniq.itemize
      end
    end
    File.open(testset.statsfile, "a") do |f|
      f.puts "\n=== APPENDED BY BENCHMARKING SCRIPT ==="
      f.puts "Analysis began: #{starttime}"
      f.puts "Analysis ended: #{endtime}"
      f.puts "Duration: #{format("%.02f", endtime-starttime)} s"
      f.puts "Goblint params: #{cmd}"
      f.puts $vrsn
    end
    testset.ok = status == 0
  end

  def run
    filename = File.basename(@path)
    cmd = "#{$goblint} #{filename} #{@params} #{ENV['gobopt']} 1>#{@testset.warnfile} --enable dbg.timing.enabled --set goblint-dir .goblint-#{@id.sub('/','-')} 2>#{@testset.statsfile}"
    starttime = Time.now
    run_testset(@testset, cmd, starttime)
  end

  def collect_warnings
    testset.collect_warnings
  end

  def compare_warnings
    testset.compare_warnings
  end

  def report
    testset.report
  end

  def heading_to_html
    "<tr><th colspan=#{html_heading.size}>#{group}</th></tr>" +
    "<tr>" +
    (html_heading.map {|h| "<th>#{h}</th>"}).join(" ") +
    "</tr>"
  end

  def to_html
    "<td>#{@id}</td>\n" +
    testset.to_html
  end

  def to_s
    "#{@name} (#{@url})"
  end
end

class ProjectIncr < Project
  attr_reader :patch_path, :conf_path
  attr_accessor :testset_incr
  @testset_incr
  def initialize(id, name, group, path, params, patch_path, conf_path)
    super(id, name, group, path, params)
    @patch_path = patch_path
    @conf_path = conf_path
    @html_heading = html_heading + ["Config", "Patched", "Size (CIL) Incr", "Checks Incr", "Time Incr", "Vars / Eval Incr", "Problems Incr"]
  end

  def create_test_set(lines)
    super(lines)
    @testset.p = self
    `patch -p0 -b <#{patch_path}`
    status = $?.exitstatus
    lines_incr = IO.readlines(path, :encoding => "UTF-8")
    `patch -p0 -b -R <#{patch_path}`
    if status != 0
      puts "Failed to apply patch: #{patch_path}"
      exit 1
    end
    @testset_incr = parse_tests(lines_incr)
    @testset_incr.p = self
    @testset_incr.warnfile = File.join($testresults, group, name + ".incr.warn.txt")
    @testset_incr.statsfile = File.join($testresults, group, name + ".incr.stats.txt")
    @testset_incr.orgfile = File.join($testresults, group, name + ".incr.c.html")
    @testset_incr.cilfile = File.join($testresults, group, name + ".incr.cil.txt")
  end

  def run
    filename = File.basename(@path)
    cmd = "#{$goblint} #{filename} #{@params} #{ENV['gobopt']} 1>#{@testset.warnfile} --enable dbg.timing.enabled --enable incremental.save --set goblint-dir .goblint-#{@id.sub('/','-')}-incr-save 2>#{@testset.statsfile}"
    cmd_incr = "#{$goblint} #{filename} #{@params} #{ENV['gobopt']} 1>#{@testset_incr.warnfile} --enable dbg.timing.enabled --enable incremental.load --set goblint-dir .goblint-#{@id.sub('/','-')}-incr-load 2>#{@testset_incr.statsfile}"
    starttime = Time.now
    run_testset(@testset_incr, cmd, starttime)
    # apply patch
    `patch -p3 -b <#{@patch_path}`
    starttime = Time.now
    run_testset(@testset_incr, cmd_incr, starttime)
    # revert patch
    `patch -p3 -b -R <#{@patch_path}`
    FileUtils.rm_rf('incremental_data')
  end

  def report
    testset.report
    `patch -p0 -b <#{patch_path}`
    testset_incr.report
    `patch -p0 -b -R <#{patch_path}`
  end

  def collect_warnings
    testset.collect_warnings
    testset_incr.collect_warnings
  end

  def compare_warnings
    testset.compare_warnings
    testset_incr.compare_warnings
  end

  def to_html
    super +
    "<td><a href=\"#{conf_path}\">#{name}</a></td>\n" +
    testset_incr.to_html
  end
end

class ProjectMarshal < Project
  def create_test_set(lines)
    super(lines)
    @testset.p = self
  end
  def run ()
    filename = File.basename(@path)
    cmd1 = "#{$goblint} #{filename} #{@params} #{ENV['gobopt']} 1>#{@testset.warnfile} --enable dbg.timing.enabled --set save_run run --set goblint-dir .goblint-#{@id.sub('/','-')}-run-save 2>#{@testset.statsfile}"
    cmd2 = "#{$goblint} #{filename} #{@params} #{ENV['gobopt']} 1>#{@testset.warnfile} --enable dbg.timing.enabled --conf run/config.json --set save_run '' --set load_run run --set goblint-dir .goblint-#{@id.sub('/','-')}-run-load 2>#{@testset.statsfile}"
    starttime = Time.now
    run_testset(@testset, cmd1, starttime)
    run_testset(@testset, cmd2, starttime)
    FileUtils.rm_rf('run')
    end
end

#processing the file information
projects = []
project_ids = Set.new
regs = Dir.open(testfiles)
regs.sort.each do |d|
  next if File.basename(d)[0] == ?.
  gid = d[0..1]
  groupname = d[3..-1]
  next unless thegroup.nil? or groupname == thegroup or # group x = only group x
    (thegroup.start_with?"-" and groupname != thegroup[1..-1]) # group -x = all groups but x
  grouppath = File.expand_path(d, testfiles)
  next unless File.directory?(grouppath)
  group = Dir.open(grouppath)
  group.sort.each do |f|
    next if File.basename(f)[0] == ?.
    next if f =~ /goblin_temp/
    next unless f =~ /^[0-9]+-.*\.c$/
    id = gid + "/" + f[0..1]
    if project_ids.member?(id) then
      puts "Duplicate test ID #{id}"
      exit 1
    end
    testname = f[3..-3]
    next unless only.nil? or testname == only
    path = File.expand_path(f, grouppath)
    lines = IO.readlines(path, :encoding => "UTF-8")

    next if not future and only.nil? and lines[0] =~ /SKIP/
    next if marshal and lines[0] =~ /NOMARSHAL/
    next if not has_linux_headers and lines[0] =~ /kernel/
    if incremental then
      config_path = File.expand_path(f[0..-3] + ".json", grouppath)
      params = "--conf #{config_path}"
    else
      lines[0] =~ /PARAM: (.*)$/
      if $1 then params = $1 else params = "" end
    end
    # always enable debugging so that the warnings would work
    params << " --set dbg.debug true"
    p = if incremental then
          patch = f[0..-3] + ".patch"
          patch_path = File.expand_path(patch, grouppath)
          conf = f[0..-3] + ".json"
          conf_path = File.expand_path(conf, grouppath)
          ProjectIncr.new(id, testname, groupname, path, params, patch_path, conf_path)
        elsif marshal then
          ProjectMarshal.new(id, testname, groupname, path, params)
        else
          Project.new(id, testname, groupname, path, params)
        end
    p.create_test_set(lines)

    projects << p
    project_ids << id
  end
end


#analysing the files
startdir = Dir.pwd
doproject = lambda do |p|
  Dir.chdir(startdir)
  filepath = p.path
  dirname = File.dirname(filepath)
  filename = File.basename(filepath)
  Dir.chdir(dirname)
  clearline
  id = "#{p.id} #{p.group}/#{p.name}"
  print "Testing #{id}"
  begin
    Dir.mkdir(File.join($testresults, p.group)) unless Dir.exist?(File.join($testresults, p.group))
  rescue
    # if we run into this, the directory was created in the time between exist? and mkdir => we can just continue
  end
  if report then
    p.report
  end
  p.run
  p
end
if sequential then
  projects = projects.map(&doproject)
else
  begin
    require 'parallel'
    # globals are protected from change when running processes instead of threads
    projects = Parallel.map(projects, &doproject)
  rescue LoadError => e
    puts "Missing parallel gem (install with: gem install parallel), falling back to sequential"
    projects = projects.map(&doproject)
  end
end
$alliswell = projects.map{|p| p.testset.ok}.all?
clearline

#Outputting
header = <<END
<head>
  <title>Tests (#{`uname -n`.chomp})</title>
  <style type="text/css">
    A:link {text-decoration: none}
    A:visited {text-decoration: none}
    A:active {text-decoration: none}
    A:hover {text-decoration: underline}
</style>
</head>
END
theresultfile = File.join($testresults, "index.html")
File.open(theresultfile, "w") do |f|
  f.puts "<html>"
  f.puts header
  f.puts "<body>"
  f.puts "<table border=2 cellpadding=4>"
  gname = ""
  projects.each do |p|
    if p.group != gname then
      gname = p.group
      f.puts p.heading_to_html
    end


    p.collect_warnings
    p.compare_warnings

    f.puts "<tr>" + p.to_html + "</tr>"

  end
  f.puts "</table>"
  f.print "<p style=\"font-size: 90%; white-space: pre-line\">"
  f.puts "Last updated: #{Time.now.strftime("%Y-%m-%d %H:%M:%S %z")}"
  f.puts "#{$vrsn}"
  f.puts "</p>"
  f.puts "</body>"
  f.puts "</html>"
end

if report then
  puts "Usage examples for high-tech script parameters: "
  puts "  Single: ./scripts/update_suite.rb simple_rc"
  puts "  Groups: ./scripts/update_suite.rb group mutex"
  puts "  Exclude group: ./scripts/update_suite.rb group -mutex"
  puts "  Future: ./scripts/update_suite.rb future"
  puts "  Force sequential execution: append -s"
  puts ("Results: " + theresultfile)
end
if $alliswell then
  puts "No errors :)".green
else
  puts "#{$failed.length} test(s) failed: #{$failed}".red
end
exit $alliswell
