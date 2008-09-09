#!/usr/bin/ruby

goblint = File.join(Dir.getwd,"goblint")
fail "Please run script from goblint dir!" unless File.exist?(goblint)

locksmith = "../others/locksmith-0.4"
runit = "timeout 300 ./cil/bin/cilly"
ls     = File.expand_path("tests/locksmith")
bench  = File.expand_path("../bench")
libc   = "#{ls}/lib.c"
nlib   = "#{ls}/newlib.c"

#processing the input file

def doit(str,f)
  lines = IO.readlines("#{str}.txt")
  res = lines.grep(/^LockSmith run for:\s*(.*) s.*$/) { |x| $1 }
  if res == [] then
    res = lines.grep(/^TIMEOUT\s*(.*) s.*$/) { |x| $1 }
    if res == [] then
      f.puts "<td><a href=\"#{str}.txt\">failed</a></td>"
    else
      f.puts "<td><a href=\"#{str}.txt\">#{res.to_s} s</a> (limit)</td>"
    end
  else
    f.puts "<td><a href=\"#{str}.txt\">#{res.to_s} s</a></td>"
  end
  races = `wc -l #{str}.short.txt`.split[0]
  f.puts "<td><a href=\"#{str}.short.txt\">#{races} races</a></td>"
end


File.open("#{ls}/ls.txt", "r") do |f|
File.open("#{ls}/index.html", "w") do |g|
  Dir.chdir(locksmith)
  g.puts "<html>"
  g.puts "<head><title>Test Results</title></head>"
  g.puts "<body>"
  g.puts "<table border=2 cellpadding=4>"
  g.puts "<tr><th>Name</th><th>Size (merged)</th>"
  ["normal", "field-sensitive"].each do |a| 
    g.puts "<th>#{a}</th>"
    g.puts "<th>warnings</th>"
  end

  i = 0
  while line = f.gets
    fn = line.chomp
    puts "Analyzing #{fn}"
    file = "#{bench}/pthread/#{fn}_comb.c"
    size = `wc -l #{file}`.split[0] + " lines"
    g.puts "<tr>"
    g.puts "<td>#{fn}</td>\n" + "<td>#{size}</td>\n"

    res_n = "#{ls}/normal/#{fn}"
    `#{runit} #{file} #{nlib} --merge --no-existentials --no-linearity 2> #{res_n}.txt`
    `grep 'Possible data race:' #{res_n}.txt > #{res_n}.short.txt`
    doit(res_n,g)

    res_f = "#{ls}/field_insensitive/#{fn}"
    `#{runit} #{file} #{nlib} --merge --no-existentials --no-linearity --field-insensitive 2> #{res_f}.txt`
    `grep 'Possible data race: &' #{res_f}.txt > #{res_f}.short.txt`
    doit(res_f,g)

    g.puts "</tr>"
  end
  g.puts "</table>"
  g.puts "</body>"
  g.puts "</html>"
end
end
