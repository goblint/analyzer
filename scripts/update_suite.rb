#!/usr/bin/ruby

require 'find'

goblint = File.join(Dir.getwd,"goblint")
fail "Please run script from goblint dir!" unless File.exist?(goblint)
testresults = File.expand_path("tests/suite_result") 
testfiles   = File.expand_path("tests/regression")

class Project
  attr_reader :name, :group, :path, :params, :warnings
  attr_writer :size
  def initialize(name, size, group, path, params, warnings)
    @name     = name
    @size     = size
    @group    = group
    @path     = path
    @params   = params
    @warnings = warnings
  end
  def to_html
    orgfile = name + ".c.txt"
    cilfile = name + ".cil.txt"
    "<td><a href=\"#{orgfile}\">#{@name}</a></td>\n" +
    "<td><a href=\"#{cilfile}\">#{@size} lines</a></td>\n"
  end
  def to_s
    "#{@name} (#{@url})"
  end
end


#Command line parameters

only = ARGV[0] unless ARGV[0].nil?
# analyses = ["mutex", "base", "cpa", "intcpa"]
# analyses = ["mutex"]

tracing = `grep 'tracing = true' src/util/messages.ml`.size > 0
if tracing then puts "Tracing in on!" else puts "Tracing is off" end

#processing the file information
projects = []
regs = Dir.open(testfiles)
regs.sort.each do |d| 
  next if File.basename(d)[0] == ?.
  groupname = d[3..-1]
  grouppath = File.expand_path(d, testfiles)
  group = Dir.open(grouppath)
  group.sort.each do |f|
    next if File.basename(f)[0] == ?.
    next if f =~ /goblin_temp/ 
    testname = f[3..-3]
    path = File.expand_path(f, grouppath)
    lines = IO.readlines(path)
    size = 0
    puts lines[0]
    lines[0] =~ /.*PARAM: (.*)$/
    if $1 then params = $1 else params = "" end
    warnings = lines.grep(/RACE|FAIL/).size
    p = Project.new(testname,size,groupname,path,params,warnings)
    projects << p 
  end
end

#analysing the files
startdir = Dir.pwd
projects.each do |p|
  Dir.chdir(startdir)
  next unless only.nil? or p.name == only 
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
  orgfile = File.join(testresults, p.name + ".c.txt")
  `cp #{filename} #{orgfile}`
  `#{goblint} #{filename} --justcil #{p.params} >#{cilfile} 2> /dev/null`
  p.size = `wc -l #{cilfile}`.split[0]
  `#{goblint} #{filename} #{p.params} 1>#{warnfile} --stats 2>#{statsfile}`
  `#{goblint} #{filename} #{p.params} --trace con 2>#{confile}` if tracing
  `#{goblint} #{filename} #{p.params} --trace sol 2>#{solfile}` if tracing
end

#Outputting
File.open(File.join(testresults, "index.html"), "w") do |f|
  f.puts "<html>"
  f.puts "<head><title>Test Results</title></head>"
  f.puts "<body>"
  f.puts "<table border=2 cellpadding=4>"
  gname = ""
  projects.each do |p|
    is_ok = true
    if p.group != gname then
      gname = p.group
      headings = ["Name", "Size (CIL)", "Warnings", "Time", "Status"]
      headings = ["Name", "Size (CIL)", "Warnings", "Time", "Constraints", "Solver", "Status"] if tracing
      f.puts "<tr><th colspan=#{headings.size}>#{gname}</th></tr>"
      f.puts "<tr>"
      headings.each {|h| f.puts "<th>#{h}</th>"}
      f.puts "</tr>"
    end
    f.puts "<tr>"
    f.puts p.to_html
    warnfile = p.name + ".warn.txt"
    warnings = 0
    File.open(File.join(testresults, warnfile), "r") do |g|
      lines = g.readlines
      warnings = lines.grep(/^.*\(.*\.c:.*\)$/).size
      if lines.grep(/does not reach the end/).size > 0 then
        warnings = (1+warnings) * (-1)
      end
      if p.warnings < 0 then 
        f.puts "<td><a href=\"#{warnfile}\">#{warnings.abs} of #{p.warnings.abs}</a>*</td>"
      else
        f.puts "<td><a href=\"#{warnfile}\">#{warnings} of #{p.warnings}</a></td>"
      end
    end
    statsfile = p.name + ".stats.txt"
    File.open(File.join(testresults, statsfile), "r") do |g|
      lines = g.readlines
      res = lines.grep(/^TOTAL\s*(.*) s.*$/) { |x| $1 }
      errors = lines.grep(/Error:/)
      if res == [] or not errors == [] then
        is_ok = false
        f.puts "<td><a href=\"#{statsfile}\">failure</a></td>"
      else
        f.puts "<td><a href=\"#{statsfile}\">#{res.to_s} s</a></td>"
      end
    end
    if tracing then
      confile = p.name + ".con.txt"
      File.open(File.join(testresults, confile), "r") do |g|
        lines = g.readlines
        cons = lines.grep(/con/).size
        f.puts "<td><a href=\"#{confile}\">#{cons} nodes</a></td>"
      end
      solfile = p.name + ".sol.txt"
      File.open(File.join(testresults, solfile), "r") do |g|
        lines = g.readlines
        sols = lines.grep(/sol: Entered/).size
        f.puts "<td><a href=\"#{solfile}\">#{sols} nodes</a></td>"
      end
    end
    
    if warnings == p.warnings && is_ok then
      f.puts "<td style =\"color: green\">PASS</td>"
    else
      f.puts "<td style =\"color: red\">FAIL</td>"
    end


    f.puts "</tr>"
  end
  f.puts "</table>"
  f.puts "</body>"
  f.puts "</html>"
end
