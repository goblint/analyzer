#!/usr/bin/ruby

locksmith = "../others/locksmith-0.4"
runit = "timeout 300 ./cil/bin/cilly"
testresults = File.expand_path("tests/ls_result")
projects    = File.expand_path("../bench")

#processing the input file

File.open("tests/ls.txt", "r") do |f|
  Dir.chdir(locksmith)
  i = 0
  while line = f.gets
    fn = line.chomp
    puts "Analyzing #{fn}"
    file = "#{projects}/pthread/#{fn}_comb.c"
    res_n = "#{testresults}/normal/#{fn}"
    res_f = "#{testresults}/field_insensitive/#{fn}"
    `#{runit} #{file} experiments/newlib.c --merge --no-existentials --no-linearity 2> #{res_n}.txt`
    `grep 'Possible data race:' #{res_n}.txt > #{res_n}.short.txt`
    `#{runit} #{file} experiments/newlib.c --merge --no-existentials --no-linearity --field-insensitive 2> #{res_f}.txt`
    `grep 'Possible data race: &' #{res_f}.txt > #{res_f}.short.txt`
  end
end
