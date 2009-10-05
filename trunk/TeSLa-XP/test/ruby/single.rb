require "rexml/document"

$LOAD_PATH << "C:\\Documents and Settings\\kornevgen\\Desktop\\tesla.2008.09.24\\TeSLa-XP\\src\\ruby"

require "tesla-mips"
require "tesla"
$initlength = 14

# must be `unsat' but `sat' occured
Runner.new.run( MIPS_MirrorSolver.new, 0, "tmpl9.xml", "data9.xml" )

