#!/usr/bin/ruby

require 'find'

goblint = File.join(Dir.getwd,"goblint")
fail "Please run script from goblint dir!" unless File.exist?(goblint)
testresults = File.expand_path("tests/suite_result") 
testfiles   = File.expand_path("tests/regression")

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
    orgfile = name + ".c.txt"
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
  gid = d[0..1]
  groupname = d[3..-1]
  grouppath = File.expand_path(d, testfiles)
  group = Dir.open(grouppath)
  group.sort.each do |f|
    next if File.basename(f)[0] == ?.
    next if f =~ /goblin_temp/ 
    id = gid + "/" + f[0..1]
    testname = f[3..-3]
    path = File.expand_path(f, grouppath)
    lines = IO.readlines(path)
    size = 0
    debug = false

    lines[0] =~ /PARAM: (.*)$/
    if $1 then params = $1 else params = "" end

    hash = Hash.new
    lines.each_with_index do |obj, i|
      i = i + 1
      next if obj =~ /^\s*\/\//
      if obj =~ /RACE/ then
        hash[i] = "race"
      elsif obj =~ /assert.*\(/ then
        if obj =~ /FAIL/ then
          hash[i] = "fail"
        elsif obj =~ /UNKNOWN/ then
          hash[i] = "unknown"
          debug = true
        else
          hash[i] = "assert"
        end
      elsif obj =~ /NOWARN/ then
        hash[i] = "nowarn"
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
  `cat -n #{filename} > #{orgfile}`
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
      next unless l =~ /^(.*)\(.*\.c:(.*)\)$/
      obj,i = $1,$2.to_i
      warnings[i] = case obj
                    when /with lockset/: "race"
                    when /will fail/   : "fail"
                    when /is unknown/  : "unknown"
                    else obj
                    end
    end
    correct = 0
    ferr = nil
    p.warnings.each_pair do |idx, type|
      case type
      when "race", "fail", "unknown", "noterm", "term"
        if warnings[idx] == type then 
          correct += 1 
        else 
          ferr = idx if ferr.nil?
        end
      when "assert", "nowarn" 
        if warnings[idx].nil? then 
          correct += 1 
        else 
          ferr = idx if ferr.nil?
        end
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
      if ferr.nil? then
        f.puts "<td style =\"color: red\">FAILED</td>"
      else
        f.puts "<td style =\"color: red\">LINE #{ferr}</td>"
      end
    end

    f.puts "</tr>"
  end
  f.puts "</table>"
  f.puts "</body>"
  f.puts "</html>"
end
