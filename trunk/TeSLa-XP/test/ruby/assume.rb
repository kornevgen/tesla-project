require "../../src/ruby/tesla.rb"

$instructionsPath = "./"
puts Runner.new.run( Solver.new, 0, IO.read("a.xml") )
