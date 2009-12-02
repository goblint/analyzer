#!/usr/bin/env ruby

  def parse_oil
    input = File.open(ARGV[0], "r")
    output = File.open(ARGV[1] + "/priorities.txt", "w")
    head = "function_of_"
    flag = true
    output.puts "default"
    while line = input.gets do
      if line =~ /(TASK|ISR)\s+(\w+).*/ then
	o_line = head + $1.downcase + "_" + $2
	flag = false
	output.puts o_line
      end
      if line =~ /PRIORITY\s*=\s*(\d+)\s*.*/ then 
	o_line = $1
	flag = true
	output.puts o_line
      end    
      if line =~ /RESOURCE\s*=\s*(\w+).*/ then 
	o_line = $1
	output.puts o_line
      end    

    end
  end

parse_oil