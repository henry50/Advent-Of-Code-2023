with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;

with Advent_IO; use Advent_IO;

with Day01;

procedure AoC is
    -- Get this from cmd line args
    Day_To_Run: Day := 1;
    -- and this
    Is_Example: Boolean := False;
    Input: Puzzle_Input;
begin
    Input := Get_Input(Day_To_Run, Is_Example);
    case Day_To_Run is
        when 1 => 
            Put_Line("Part 1:");
            Put(Day01.Part1(Input)); New_Line;
            Put_Line("Part 2:");
            Put(Day01.Part2(Input)); New_Line;
        when others =>
            Put_Line("Not implemented");
    end case;
exception
    when E: File_Not_Found_Error =>
        Put_Line("Unable to open input file: " & Exception_Message(E));
end AoC;