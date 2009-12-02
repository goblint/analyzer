#!/usr/bin/env ruby

  def parse_trampoline
    input = File.open(ARGV[0], "r")
    output = File.open(ARGV[1] + "/resources.txt", "w")  
    while line = input.gets do
      if line =~ / .*resource_id_of_(\w+)\s+(\d+).*/ then
	output.puts $2
	output.puts $1
      end
    end
  end

parse_trampoline