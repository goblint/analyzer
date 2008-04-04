#!/usr/bin/ruby

locksmith = "../others/locksmith-0.4"
runit = "timeout 300 ./cil/bin/cilly"
ls     = File.expand_path("tests/locksmith")
bench  = File.expand_path("../bench")
libc   = "#{ls}/lib.c"
nlib   = "#{ls}/newlib.c"

#processing the input file

File.open("#{ls}/ls.txt", "r") do |f|
  Dir.chdir(locksmith)
  i = 0
  while line = f.gets
    fn = line.chomp
    puts "Analyzing #{fn}"
    file = "#{bench}/pthread/#{fn}_comb.c"
    res_n = "#{ls}/normal/#{fn}"
    res_f = "#{ls}/field_insensitive/#{fn}"
    `#{runit} #{file} #{nlib} --merge --no-existentials --no-linearity 2> #{res_n}.txt`
    `grep 'Possible data race:' #{res_n}.txt > #{res_n}.short.txt`
    `#{runit} #{file} #{nlib} --merge --no-existentials --no-linearity --field-insensitive 2> #{res_f}.txt`
    `grep 'Possible data race: &' #{res_f}.txt > #{res_f}.short.txt`
  end
end
