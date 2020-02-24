#!/usr/bin/ruby

require 'set'

(puts "missing arguments"; exit) unless ARGV.length == 2
gb_res = ARGV[0]
tr_res = ARGV[1]

gb_vars = Set.new
IO.foreach gb_res do |line|
  if line =~ /over variable "(.*)"/ then
    var = $1
    gb_vars << var
  end
end

tr_vars = Set.new
IO.foreach tr_res do |line|
  if line =~ /\[(.*)\]/ then
    var = $1
    tr_vars << var
  end
end

agree = gb_vars & tr_vars
puts "They agree about the following #{agree.length} variables:"
puts agree.to_a
puts

only_gb = gb_vars - tr_vars
puts "Only Goblint complains about the following #{only_gb.length} variables:"
puts only_gb.to_a
puts

only_tr = tr_vars - gb_vars
puts "Only Trier complains about the following #{only_tr.length} variables:"
puts only_tr.to_a
puts

puts "---- SUMMARY ----"
puts "Agreement: #{agree.length}"
puts "Goblint:   #{only_gb.length}"
puts "Trier:     #{only_tr.length}"
puts "OK: #{agree.length} / Goblint: #{only_gb.length} / Trier: #{only_tr.length}"
