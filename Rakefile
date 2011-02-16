task :default => [:run, :cat, :clean]

def cputs(str)
	puts str if str.strip.length > 0
end

task :run do |t, args|
	cputs `scala -unchecked toy.scala`
end

task :cat do |t, args|
	Dir['*.log'].sort.each do |filename|
		cputs File.read filename
	end
end

task :clean do |t, args|
	cputs `rm -f {0,1,2,3,4,5,6,7,8,9}.log`
end
