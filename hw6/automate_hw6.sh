#!/usr/bin/env bash
#
# Script to run homework 6 tests in the background
# **Will use a lot of CPU Power**
#
output_file="homework_6_result_asus.txt"
echo "Start of Homework 6 Result file" > $output_file
javac James_homework6.java
max_prime=$1
filter_counter=100
queue_counter=100
while [[ $filter_counter -le $max_prime ]]; do
	while [[ $queue_counter -le $max_prime ]]; do
	#	echo "Running HW6..."
		java HW6 $max_prime $filter_counter $queue_counter >> $output_file
	#	echo "Incrementing QueueSize..."
		queue_counter=$((queue_counter*10))
	done
#	echo "Incrementing FilterSize..."
	queue_counter=100
	filter_counter=$((filter_counter*10))
done
cat $output_file | grep R	
