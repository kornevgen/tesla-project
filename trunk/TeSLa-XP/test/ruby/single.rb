$LOAD_PATH << "/home/kornevgen/diss/workspace/TeSLa-XP/src/ruby"

$TAGSETLEN = 31
$PFNLEN = 24
$TAGLEN = $PFNLEN
$L1ASSOC = 4
$TLBASSOC = 4
$SEGBITS = 40
$PABITS = 36
$MASK = 0

require "tesla-mips"
require "tesla"

$instructionsPath = "./"

Runner.new.run( MIPS_CombinedSolver.new, 0, IO.read("template.xml"), IO.read("data.xml") )

