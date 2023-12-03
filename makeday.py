ADS = """\
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Advent_IO; use Advent_IO;
package Day{} is

function Part1(Input: Puzzle_Input) return Integer;
function Part2(Input: Puzzle_Input) return Integer;

end Day{};
"""

ADB = """\
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Advent_IO; use Advent_IO;

package body Day{} is

function Part1(Input: Puzzle_Input) return Integer is

begin
    return 0;
end Part1;

function Part2(Input: Puzzle_Input) return Integer is
    
begin
    return 0;
end Part2;

end Day{};
"""

CASE = """\
        when {} => 
            Put_Line("Part 1:");
            Put(Day{}.Part1(Input)); New_Line;
            Put_Line("Part 2:");
            Put(Day{}.Part2(Input)); New_Line;
"""

def make_files():
    for i in range(1, 26):
        num = f"{i:02}"
        with open(f"Day{num}.ads", "w") as f:
            f.write(ADS.format(num, num))
        with open(f"Day{num}.adb", "w") as f:
            f.write(ADB.format(num, num))

def make_case():
    with open("aoc_cases.txt", "w") as f:
        for i in range(1, 26):
            num = f"{i:02}"
            f.write(CASE.format(str(i), num, num))
   
# make_files()         
# make_case()