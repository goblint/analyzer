#!/usr/bin/ruby

goblint = File.join(Dir.getwd,"goblint")
fail "Please run script from goblint dir!" unless File.exist?(goblint)
testresults = File.expand_path("tests/suite_result") + "/"

class Project
  attr_reader :name, :group, :path, :params, :warnings
  attr_writer :size
  def initialize(name, size, group, path, params, warnings)
    @name     = name
    @url      = "http://goblin.at.mt.ut.ee/goblint/tracker/browser/trunk/goblint/" + path
    @size     = size
    @group    = group
    @path     = path
    @params   = params
    @warnings = warnings
  end
  def to_html
    cilfile = name + ".txt"
    "<td><a href=\"#{@url}\">#{@name}</a></td>\n" +
    "<td><a href=\"#{cilfile}\">#{@size}</a></td>\n"
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

#processing the input file

projects = []
File.open("tests/suite.txt", "r") do |f|
  i = 0
  while line = f.gets
    next if line =~ /^\s*$/ 
    if line =~ /Group: (.*)/
      gname = $1
      next
    end
    name = line.chomp
    path = f.gets.chomp
    params = f.gets.chomp
    params = "" if params == "-"
    warnings = f.gets.chomp.to_i
    next if name =~ /#.*/
    p = Project.new(name,0,gname,path,params,warnings)
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
  warnfile = testresults + p.name + ".warn.txt"
  statsfile = testresults + p.name + ".stats.txt"
  confile = testresults + p.name + ".con.txt"
  solfile = testresults + p.name + ".sol.txt"
  cilfile = testresults + p.name + ".txt"
  `#{goblint} #{filename} --justcil #{p.params} >#{cilfile} 2> /dev/null`
  p.size = `wc -l #{cilfile}`.split[0] + " lines"
  `#{goblint} #{filename} --debug #{p.params} 1>#{warnfile} --stats 2>#{statsfile}`
  `#{goblint} #{filename} #{p.params} --trace con 2>#{confile}` if tracing
  `#{goblint} #{filename} #{p.params} --trace sol 2>#{solfile}` if tracing
end

File.open(testresults + "index.html", "w") do |f|
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
    File.open(testresults + warnfile, "r") do |g|
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
    File.open(testresults + statsfile, "r") do |g|
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
      File.open(testresults + confile, "r") do |g|
        lines = g.readlines
        cons = lines.grep(/con/).size
        f.puts "<td><a href=\"#{confile}\">#{cons} nodes</a></td>"
      end
      solfile = p.name + ".sol.txt"
      File.open(testresults + solfile, "r") do |g|
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
