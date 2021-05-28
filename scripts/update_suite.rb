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
timeout = 5 # seconds

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

goblint = File.join(Dir.getwd,"goblint")
goblintbyte = File.join(Dir.getwd,"goblint.byte")
if File.exists?(goblintbyte) then
  puts "Running the byte-code version! Continue? (y/n)"
  exit unless $stdin.gets()[0] == 'y'
  goblint = goblintbyte
elsif not File.exist?(goblint) then
  fail "Goblint not present in working directory. Please run script from goblint dir!"
end
vrsn = `#{goblint} --version`

if not File.exists? "linux-headers" then
  puts "Missing linux-headers, will download now!"
  `make headers`
end

testresults = File.expand_path("tests/suite_result")
testfiles   = File.expand_path("tests/regression")

alliswell = true
failed    = [] # failed tests
timedout  = [] # timed out tests

class Project
  attr_reader :id, :name, :group, :path, :params, :tests, :tests_line, :todo
  attr_accessor :size, :ok
  def initialize(id, name, size, group, path, params, tests, tests_line, todo, ok)
    @id       = id
    @name     = name
    @size     = size
    @group    = group
    @path     = path
    @params   = params
    @tests = tests
    @tests_line = tests_line
    @todo = todo
    @ok = ok
  end
  def to_html
    orgfile = File.join(group, name + ".c.html")
    cilfile = File.join(group, name + ".cil.txt")
    "<td>#{@id}</td>\n" +
    "<td><a href=\"#{orgfile}\">#{@name}</a></td>\n" +
    "<td><a href=\"#{cilfile}\">#{@size} lines</a></td>\n"
  end
  def to_s
    "#{@name} (#{@url})"
  end
end

#Command line parameters
#Either only run a single test, or
#"future" will also run tests we normally skip
dump = ARGV.last == "-d" && ARGV.pop
sequential = ARGV.last == "-s" && ARGV.pop
marshal = ARGV.last == "-m" && ARGV.pop
report = ARGV.last == "-r" && ARGV.pop
only = ARGV[0] unless ARGV[0].nil?
if marshal then
  sequential = true
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

# tracing = `grep 'tracing = true' src/config.ml`.size > 0
# if tracing then puts "Tracing in on!" else puts "Tracing is off" end

#processing the file information
projects = []
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
    testname = f[3..-3]
    next unless only.nil? or testname == only
    path = File.expand_path(f, grouppath)
    lines = IO.readlines(path)
    debug = true

    next if not future and only.nil? and lines[0] =~ /SKIP/
    next if marshal and lines[0] =~ /NOMARSHAL/
    debug = false unless lines[0] =~ /DEBUG/
    lines[0] =~ /PARAM: (.*)$/
    if $1 then params = $1 else params = "" end

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
      elsif obj =~ /assert.*\(/ then
        debug = true
        if obj =~ /FAIL/ then
          tests[i] = "fail"
        elsif obj =~ /UNKNOWN!/ then
          tests[i] = "unknown!"
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
      debug = true
    when /TERM/
      tests[-1] = "term"
      debug = true
    end
    params << " --set dbg.debug true" if debug
    p = Project.new(id, testname, 0, groupname, path, params, tests, tests_line, todo, true)
    projects << p
  end
end

highlighter = lambda {|f,o| "cp #{f} #{o}"}
if report then
  cmds = {"code2html" => lambda {|f,o| "code2html -l c -n #{f} 2> /dev/null 1> #{o}"},
          "source-highlight" => lambda {|f,o| "source-highlight -n -i #{f} -o #{o}"},
          "pygmentize" => lambda {|f,o| "pygmentize -O full,linenos=1 -o #{o} #{f}"}
         }
  cmds.each do |name, cmd|
    # if `which #{cmd} 2> /dev/null`.empty? then
    if ENV['PATH'].split(':').map {|f| File.executable? "#{f}/#{name}"}.include?(true) then
      highlighter = cmd
      break
    end
  end
  if highlighter.nil? then
    puts "Warning: No syntax highlighter installed (code2html, source-highlight, pygmentize)."
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
    Dir.mkdir(File.join(testresults, p.group)) unless Dir.exist?(File.join(testresults, p.group))
  rescue
    # if we run into this, the directory was created in the time between exist? and mkdir => we can just continue
  end
  warnfile = File.join(testresults, p.group, p.name + ".warn.txt")
  statsfile = File.join(testresults, p.group, p.name + ".stats.txt")
#   confile = File.join(testresults, p.group, p.name + ".con.txt")
#   solfile = File.join(testresults, p.group, p.name + ".sol.txt")
  cilfile = File.join(testresults, p.group, p.name + ".cil.txt")
  orgfile = File.join(testresults, p.group, p.name + ".c.html")
  if report then
    system(highlighter.call(filename, orgfile))
    `#{goblint} #{filename} --set justcil true #{p.params} >#{cilfile} 2> /dev/null`
    p.size = `wc -l #{cilfile}`.split[0]
  end
  starttime = Time.now
  if marshal then
    cmd = "#{goblint} #{filename} #{p.params} #{ENV['gobopt']} 1>#{warnfile} --sets warnstyle \"legacy\" --set printstats true --sets save_run run  2>#{statsfile}"
  else
    cmd = "#{goblint} #{filename} #{p.params} #{ENV['gobopt']} 1>#{warnfile} --sets warnstyle \"legacy\" --set printstats true 2>#{statsfile}"
  end
  pid = Process.spawn(cmd, :pgroup=>true)
  begin
    Timeout::timeout(timeout) {Process.wait pid}
  rescue Timeout::Error
    pgid = Process.getpgid(pid)
    puts "\t #{id} reached timeout of #{timeout}s!".red + " Killing pgid #{pgid}..."
    timedout.push id
    Process.kill('KILL', -1*pgid)
    p.ok = false
    return p
  end
  endtime   = Time.now
  status = $?.exitstatus
  if status != 0 then
    reason = if status == 1 then "error" elsif status == 2 then "exception" elsif status == 3 then "verify" end
    clearline
    puts "Testing #{id}" + "\t Status: #{status} (#{reason})".red
    stats = File.readlines statsfile
    if status == 1 then
      puts stats.last(5).itemize
    elsif status == 2 then # if stats[0] =~ /exception/ then
      lastline = (File.readlines warnfile).last()
      puts lastline.strip().sub filename, relpath(filepath).to_s unless lastline.nil?
      puts stats[0..9].itemize
    elsif status == 3 then
      warn = File.readlines warnfile
      puts (warn.select { |x| x["Unsatisfied constraint"] || x["Fixpoint not reached"] }).uniq.itemize
    end
  end
#   `#{goblint} #{filename} #{p.params} --trace con 2>#{confile}` if tracing
#   `#{goblint} #{filename} #{p.params} --trace sol 2>#{solfile}` if tracing
  File.open(statsfile, "a") do |f|
    f.puts "\n=== APPENDED BY BENCHMARKING SCRIPT ==="
    f.puts "Analysis began: #{starttime}"
    f.puts "Analysis ended: #{endtime}"
    f.puts "Duration: #{format("%.02f", endtime-starttime)} s"
    f.puts "Goblint params: #{cmd}"
    f.puts vrsn
  end
  if marshal then
    cmd = "#{goblint} #{filename} #{p.params} #{ENV['gobopt']} 1>#{warnfile} --sets warnstyle \"legacy\" --set printstats true --conf run/config.json --sets save_run '' --sets load_run run  2>#{statsfile}"
    pid = Process.spawn(cmd, :pgroup=>true)
    begin
      Timeout::timeout(timeout) {Process.wait pid}
    rescue Timeout::Error
      pgid = Process.getpgid(pid)
      puts "\t #{id} reached timeout of #{timeout}s!".red + " Killing pgid #{pgid}..."
      timedout.push id
      Process.kill('INT', -1*pgid)
      p.ok = false
      return p
    end
    endtime   = Time.now
    status = $?.exitstatus
    if status != 0 then
      reason = if status == 1 then "error" elsif status == 2 then "exception" elsif status == 3 then "verify" end
      clearline
      puts "Testing #{id}" + "\t Status: #{status} (#{reason})".red
      stats = File.readlines statsfile
      if status == 1 then
        puts stats.last(5).itemize
      elsif status == 2 then # if stats[0] =~ /exception/ then
        lastline = (File.readlines warnfile).last()
        puts lastline.strip().sub filename, relpath(filepath).to_s unless lastline.nil?
        puts stats[0..9].itemize
      elsif status == 3 then
        warn = File.readlines warnfile
        puts (warn.select { |x| x["Unsatisfied constraint"] || x["Fixpoint not reached"] }).uniq.itemize
      end
    end
    #   `#{goblint} #{filename} #{p.params} --trace con 2>#{confile}` if tracing
    #   `#{goblint} #{filename} #{p.params} --trace sol 2>#{solfile}` if tracing
    File.open(statsfile, "a") do |f|
      f.puts "\n=== APPENDED BY BENCHMARKING SCRIPT ==="
      f.puts "Analysis began: #{starttime}"
      f.puts "Analysis ended: #{endtime}"
      f.puts "Duration: #{format("%.02f", endtime-starttime)} s"
      f.puts "Goblint params: #{cmd}"
      f.puts vrsn
    end
    FileUtils.rm_rf('run')
  end
  p.ok = status == 0
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
    puts "Missing dependency. Please run: gem install parallel"
    raise e
  end
end
alliswell = projects.map{|p| p.ok}.all?
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
theresultfile = File.join(testresults, "index.html")
File.open(theresultfile, "w") do |f|
  f.puts "<html>"
  f.puts header
  f.puts "<body>"
  f.puts "<table border=2 cellpadding=4>"
  gname = ""
  projects.each do |p|
    id = "#{p.id} #{p.group}/#{p.name}"
    is_ok = true
    if p.group != gname then
      gname = p.group
      headings = ["ID", "Name", "Size (CIL)", "Checks", "Time", "Problems"]
#       headings = ["ID", "Name", "Size (CIL)", "Checks", "Time", "Constraints", "Solver", "Problems"] if tracing
      f.puts "<tr><th colspan=#{headings.size}>#{gname}</th></tr>"
      f.puts "<tr>"
      headings.each {|h| f.puts "<th>#{h}</th>"}
      f.puts "</tr>"
    end
    f.puts "<tr>"
    f.puts p.to_html

    warnfile = File.join(p.group, p.name + ".warn.txt")
    warnings = Hash.new
    warnings[-1] = "term"
    lines = IO.readlines(File.join(testresults, warnfile))
    lines.each do |l|
      if l =~ /does not reach the end/ then warnings[-1] = "noterm" end
      next unless l =~ /(.*)\(.*\:(.*)\)/
      obj,i = $1,$2.to_i

      ranking = ["other", "warn", "race", "norace", "deadlock", "nodeadlock", "success", "fail", "unknown", "term", "noterm"]
      thiswarn =  case obj
                    when /lockset:/                  then "race"
                    when /Deadlock/                  then "deadlock"
                    when /Assertion .* will fail/    then "fail"
                    when /Assertion .* will succeed/ then "success"
                    when /Assertion .* is unknown/   then "unknown"
                    when /Uninitialized/             then "warn"
                    when /dereferencing of null/     then "warn"
                    when /CW:/                       then "warn"
                    when /Fixpoint not reached/      then "warn"
                    when /.*file handle.*/           then "warn"
                    when /.*file is never closed/    then "warn"
                    when /.*unclosed files: .*/      then "warn"
                    when /changed pointer .*/        then "warn"
                    else "other"
                  end
      oldwarn = warnings[i]
      if oldwarn.nil? then
        warnings[i] = thiswarn
      else
        warnings[i] = ranking[[ranking.index(thiswarn), ranking.index(oldwarn)].max]
      end
    end
    correct = 0
    ignored = 0
    ferr = nil
    path = relpath(p.path) # full p.path is too long and p.name does not allow click to open in terminal
    p.tests.each_pair do |idx, type|
      check = lambda {|cond|
        if cond then
          correct += 1
          if p.todo.include? idx then puts "Excellent: ignored check on #{path.to_s.cyan}:#{idx.to_s.blue} is now passing!" end
        else
          if p.todo.include? idx then ignored += 1 else
            puts "Expected #{type.yellow}, but registered #{(warnings[idx] or "nothing").yellow} on #{p.name.cyan}:#{idx.to_s.blue}"
            puts p.tests_line[idx].rstrip.gray
            ferr = idx if ferr.nil? or idx < ferr
          end
        end
      }
      case type
      when "unknown"
        check.call ["deadlock", "race", "fail", "unknown", "noterm", "term", "warn", "success"].include? warnings[idx] 
      when "deadlock", "race", "fail", "noterm", "unknown!", "term", "warn"
        check.call warnings[idx] == type.tr('!', '')
      when "nowarn"
        check.call warnings[idx].nil?
      when "assert"
        check.call warnings[idx] == "success"
      when "norace"
        check.call warnings[idx] != "race"
      when "nodeadlock"
        check.call warnings[idx] != "deadlock"
      end
    end
    f.puts "<td><a href=\"#{warnfile}\">#{correct} of #{p.tests.size}</a></td>"

    statsfile = File.join(p.group, p.name + ".stats.txt")
    lines = IO.readlines(File.join(testresults, statsfile))
    res = lines.grep(/^TOTAL\s*(.*) s.*$/) { $1 }
    errors = lines.grep(/Error:/)
    if res == [] or not errors == [] then
      is_ok = false
      f.puts "<td><a href=\"#{statsfile}\">failure</a></td>"
    else
      f.puts "<td><a href=\"#{statsfile}\">#{"%.2f" % res} s</a></td>"
    end

#     if tracing then
#       confile = p.name + ".con.txt"
#       lines = IO.readlines(File.join(testresults, confile))
#       cons = lines.grep(/con/).size
#       f.puts "<td><a href=\"#{confile}\">#{cons} nodes</a></td>"
#       solfile = p.name + ".sol.txt"
#       lines = IO.readlines(File.join(testresults, solfile))
#       sols = lines.grep(/sol: Entered/).size
#       f.puts "<td><a href=\"#{solfile}\">#{sols} nodes</a></td>"
#     end

    if correct + ignored == p.tests.size && is_ok then
      f.puts "<td style =\"color: green\">NONE</td>"
    else
      alliswell = false
      if not timedout.include? id then
        failed.push "#{p.id} #{p.name}"
        exc = if lines[0] =~ /exception/ then " (see exception above)" else "" end
        puts "#{id}" + " failed#{exc}!".red
        puts ""
        if dump then
          puts "============== WARNINGS ==============="
          puts File.read(File.join(testresults, warnfile))
          puts "================ STATS ================"
          puts File.read(File.join(testresults, statsfile))
          puts "======================================="
        end
      end
      if not is_ok or ferr.nil? then
        f.puts "<td style =\"color: red\">FAILED</td>"
      else
        whataglorifiedmess = File.join(p.group, p.name + ".c.html")
        f.puts "<td><a href=\"#{whataglorifiedmess}#line#{ferr}\" style =\"color: red\">LINE #{ferr}</a></td>"
      end
    end

    f.puts "</tr>"
  end
  f.puts "</table>"
  f.print "<p style=\"font-size: 90%; white-space: pre-line\">"
  f.puts "Last updated: #{Time.now.strftime("%Y-%m-%d %H:%M:%S %z")}"
  f.puts "#{vrsn}"
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
if alliswell then
  puts "No errors :)".green
else
  puts "#{failed.length} tests failed: #{failed}".red
end
exit alliswell
