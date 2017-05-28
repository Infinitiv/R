#!/usr/bin/ruby
require 'date'
printf "Enter date (default is #{Time.now.to_date}): "
date = gets.chomp
date = Time.now.to_date if date.empty?
printf "Enter wheight: "
wheight = gets.chomp
printf "Enter heartrate: "
heartrate = gets.chomp
File.open("./self.csv", "a"){|f| f.write("#{[date, wheight, heartrate].join(",")}\r\n")}
%x(R -f ./self.R)
