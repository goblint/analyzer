#!/usr/bin/ruby

require 'find'
require 'fileutils'

goblint = File.join(Dir.getwd,"goblint")
goblintbyte = File.join(Dir.getwd,"goblint.byte")
if File.exists?(goblintbyte) then
  puts "Running the byte-code version!"
  `make byte`
  goblint = goblintbyte if File.exists?(goblintbyte)
else
  fail "Please run script from goblint dir!" unless File.exist?(goblint)
  `make`
end
$rev = `git describe --tags`.chomp + `git log -1 --pretty=format:' (%ai)'`

backup = File.join(Dir.getwd,"goblint.script_backup.json")
json   = File.join(Dir.getwd, "goblint.json")
FileUtils.mv(json, backup) if File.exists?(json) 

testresults = File.expand_path("tests/suite_result") 
testfiles   = File.expand_path("tests/regression")

alliswell = true

class Project
  attr_reader :name, :group, :path, :params, :warnings
  attr_writer :size
  def initialize(id, name, size, group, path, params, warnings)
    @id       = id
    @name     = name
    @size     = size
    @group    = group
    @path     = path
    @params   = params
    @warnings = warnings
  end
  def to_html
    orgfile = name + ".c.html"
    cilfile = name + ".cil.txt"
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
only = ARGV[0] unless ARGV[0].nil?
if only == "future" then
  future = true
  only = nil
elsif only == "group" then
  future = true
  thegroup = ARGV[1]
  only = nil
else
  future = false
end
# analyses = ["mutex", "base", "cpa", "intcpa"]
# analyses = ["mutex"]

tracing = `grep 'tracing = true' src/util/messages.ml`.size > 0
if tracing then puts "Tracing in on!" else puts "Tracing is off" end

#processing the file information
projects = []
regs = Dir.open(testfiles)
regs.sort.each do |d| 
  next if File.basename(d)[0] == ?.
  gid = d[0..1]
  groupname = d[3..-1]
  next unless thegroup.nil? or groupname == thegroup
  grouppath = File.expand_path(d, testfiles)
  group = Dir.open(grouppath)
  group.sort.each do |f|
    next if File.basename(f)[0] == ?.
    next if f =~ /goblin_temp/ 
    next unless f =~ /.*\.c$/ 
    id = gid + "/" + f[0..1]
    testname = f[3..-3]
    next unless only.nil? or testname == only 
    path = File.expand_path(f, grouppath)
    lines = IO.readlines(path)
    size = 0
    debug = true

    next if not future and only.nil? and lines[0] =~ /SKIP/
    debug = false unless lines[0] =~ /DEBUG/
    lines[0] =~ /PARAM: (.*)$/
    if $1 then params = $1 else params = "" end
      
    hash = Hash.new
    i = 0
    lines.each do |obj|
      i = i + 1
      if obj =~ /#line ([0-9]+).*$/ then
        i = $1.to_i - 1
      end
       next if obj =~ /^\/\//
      if obj =~ /RACE/ then
        hash[i] = if obj =~ /NORACE/ then "norace" else "race" end
      elsif obj =~ /assert.*\(/ then
        debug = true
        if obj =~ /FAIL/ then
          hash[i] = "fail"
        elsif obj =~ /UNKNOWN/ then
          hash[i] = "unknown"
        else
          hash[i] = "assert"
        end
      elsif obj =~ /NOWARN/ then
        hash[i] = "nowarn"
      elsif obj =~ /WARN/ then
        hash[i] = "warn"
      end
    end
    case lines[0]
    when /NON?TERM/ 
      hash[-1] = "noterm"
      debug = true
    when /TERM/: 
      hash[-1] = "term"
      debug = true
    end
    params << " --debug" if debug
    p = Project.new(id,testname,size,groupname,path,params,hash)
    projects << p 
  end
end

#analysing the files
startdir = Dir.pwd
projects.each do |p|
  Dir.chdir(startdir)
  filepath = p.path
  dirname = File.dirname(filepath)
  filename = File.basename(filepath)
  Dir.chdir(dirname)
  puts "Analysing #{p.name}"
  warnfile = File.join(testresults, p.name + ".warn.txt")
  statsfile = File.join(testresults, p.name + ".stats.txt")
  confile = File.join(testresults, p.name + ".con.txt")
  solfile = File.join(testresults, p.name + ".sol.txt")
  cilfile = File.join(testresults, p.name + ".cil.txt")
  orgfile = File.join(testresults, p.name + ".c.html")
  `code2html -l c -n #{filename} > #{orgfile}`
  `#{goblint} #{filename} --justcil #{p.params} >#{cilfile} 2> /dev/null`
  p.size = `wc -l #{cilfile}`.split[0]
  starttime = Time.now
  cmd = "#{goblint} #{filename} #{p.params} 1>#{warnfile} --stats 2>#{statsfile}"
  system(cmd)
  endtime   = Time.now
  #status = $?.exitstatus
  `#{goblint} #{filename} #{p.params} --trace con 2>#{confile}` if tracing
  `#{goblint} #{filename} #{p.params} --trace sol 2>#{solfile}` if tracing
  File.open(statsfile, "a") do |f|
    f.puts "\n=== APPENDED BY BENCHMARKING SCRIPT ==="
    f.puts "Analysis began: #{starttime}"
    f.puts "Analysis ended: #{endtime}"
    f.puts "Duration: #{format("%.02f", endtime-starttime)} s"
    f.puts "Git describe: #{$rev}"
    f.puts "Goblint params: #{cmd}"
  end
end
FileUtils.mv(backup,json) if File.exists?(backup) 

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
File.open(File.join(testresults, "index.html"), "w") do |f|
  f.puts "<html>"
  f.puts header
  f.puts "<body>"
  f.puts "<table border=2 cellpadding=4>"
  gname = ""
  projects.each do |p|
    is_ok = true
    if p.group != gname then
      gname = p.group
      headings = ["ID", "Name", "Size (CIL)", "Checks", "Time", "Problems"]
      headings = ["ID", "Name", "Size (CIL)", "Checks", "Time", "Constraints", "Solver", "Problems"] if tracing
      f.puts "<tr><th colspan=#{headings.size}>#{gname}</th></tr>"
      f.puts "<tr>"
      headings.each {|h| f.puts "<th>#{h}</th>"}
      f.puts "</tr>"
    end
    f.puts "<tr>"
    f.puts p.to_html

    warnfile = p.name + ".warn.txt"
    warnings = Hash.new
    warnings[-1] = "term"
    lines = IO.readlines(File.join(testresults, warnfile))
    lines.each do |l| 
      if l =~ /does not reach the end/ then warnings[-1] = "noterm" end
      next unless l =~ /(.*)\(.*\:(.*)\)/
      obj,i = $1,$2.to_i

      ranking = ["other", "warn", "race", "norace", "assert", "fail", "unknown", "term", "noterm"]
      thiswarn =  case obj
                    when /with lockset:/: "race"
                    when /will fail/    : "fail"
                    when /is unknown/   : "unknown"
                    when /Uninitialized/ : "warn"
                    when /dereferencing of null/ : "warn"
                    when /CW:/ : "warn"
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
    ferr = nil
    p.warnings.each_pair do |idx, type|
      case type
      when "race", "fail", "unknown", "noterm", "term", "warn"
        if warnings[idx] == type then 
          correct += 1 
        else 
          #puts "Expected #{type}, but registered #{warnings[idx]} on #{p.name}:#{idx}"
          ferr = idx if ferr.nil? or idx < ferr
        end
      when "nowarn" 
        if warnings[idx].nil? then correct += 1 
        else ferr = idx if ferr.nil? or idx < ferr end
      when "assert" 
        if warnings[idx] != "fail"  and warnings[idx] != "unknown" then correct += 1 
        else ferr = idx if ferr.nil? or idx < ferr end
      when "norace"
        if warnings[idx] != "race" then correct += 1 
        else ferr = idx if ferr.nil? or idx < ferr end
      end
    end
    f.puts "<td><a href=\"#{warnfile}\">#{correct} of #{p.warnings.size}</a></td>"

    statsfile = p.name + ".stats.txt"
    lines = IO.readlines(File.join(testresults, statsfile))
    res = lines.grep(/^TOTAL\s*(.*) s.*$/) { |x| $1 }
    errors = lines.grep(/Error:/)
    if res == [] or not errors == [] then
      is_ok = false
      f.puts "<td><a href=\"#{statsfile}\">failure</a></td>"
    else
      f.puts "<td><a href=\"#{statsfile}\">#{res.to_s} s</a></td>"
    end

    if tracing then
      confile = p.name + ".con.txt"
      lines = IO.readlines(File.join(testresults, confile))
      cons = lines.grep(/con/).size
      f.puts "<td><a href=\"#{confile}\">#{cons} nodes</a></td>"
      solfile = p.name + ".sol.txt"
      lines = IO.readlines(File.join(testresults, solfile))
      sols = lines.grep(/sol: Entered/).size
      f.puts "<td><a href=\"#{solfile}\">#{sols} nodes</a></td>"
    end
    
    if correct == p.warnings.size && is_ok then
      f.puts "<td style =\"color: green\">NONE</td>"
    else
      alliswell = false
      if ferr.nil? then
        f.puts "<td style =\"color: red\">FAILED</td>"
      else
        whataglorifiedmess = p.name + ".c.html"
        f.puts "<td><a href=\"#{whataglorifiedmess}#line#{ferr}\" style =\"color: red\">LINE #{ferr}</a></td>"
      end
    end

    f.puts "</tr>"
  end
  f.puts "</table>"
  f.puts "</body>"
  f.puts "</html>"
end

if alliswell then puts "All is well!" else puts "All is not well!" end
exit alliswell
