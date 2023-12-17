with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Command_Line;

with Advent_IO; use Advent_IO;

with Day01; with Day02; with Day03; with Day04; with Day05;
with Day06; with Day07; with Day08; with Day09; with Day10;
with Day11; with Day12; with Day13; with Day14; with Day15;
with Day16; with Day17; with Day18; with Day19; with Day20;
with Day21; with Day22; with Day23; with Day24; with Day25;

procedure AoC is
    Day_To_Run: Day;
    Input_File: Unbounded_String;
    Input: Puzzle_Input;
    type Input_Method_Enum is (Auto, Manual);
    Input_Method: Input_Method_Enum;
    package ACL renames Ada.Command_Line;
    function Pad_Day(D: Day) return String is
        Day_Image: String := D'Image;
    begin
        if D < 10 then
            return "0" & Day_Image(2);
        else
            return Day_Image(2..3);
        end if;
    end Pad_Day;
begin
    Input_Method := (case ACL.Argument_Count is
        when 1 => Auto,
        when 2 => Manual,
        when others => raise Program_Error with "Usage: " & ACL.Command_Name & " day [input_file]"
    );
    Day_To_Run := Day'Value(ACL.Argument(1));
    Input_File := To_Unbounded_String(case Input_Method is
        when Auto => "./input/day" & Pad_Day(Day_To_Run) & ".txt",
        when Manual => "./input/" & ACL.Argument(2)
    );
    Input := Get_Input(To_String(Input_File));
    case Day_To_Run is
        when 1 => 
            Put_Line("Part 1:");
            Put_Line(Day01.Part1(Input));
            Put_Line("Part 2:");
            Put_Line(Day01.Part2(Input));
        when 2 => 
            Put_Line("Part 1:");
            Put_Line(Day02.Part1(Input));
            Put_Line("Part 2:");
            Put_Line(Day02.Part2(Input));
        when 3 => 
            Put_Line("Part 1:");
            Put_Line(Day03.Part1(Input));
            Put_Line("Part 2:");
            Put_Line(Day03.Part2(Input));
        when 4 => 
            Put_Line("Part 1:");
            Put_Line(Day04.Part1(Input));
            Put_Line("Part 2:");
            Put_Line(Day04.Part2(Input));
        when 5 => 
            Put_Line("Part 1:");
            Put_Line(Day05.Part1(Input));
            Put_Line("Part 2:");
            Put_Line(Day05.Part2(Input));
        when 6 => 
            Put_Line("Part 1:");
            Put_Line(Day06.Part1(Input));
            Put_Line("Part 2:");
            Put_Line(Day06.Part2(Input));
        when 7 => 
            Put_Line("Part 1:");
            Put_Line(Day07.Part1(Input));
            Put_Line("Part 2:");
            Put_Line(Day07.Part2(Input));
        when 8 => 
            Put_Line("Part 1:");
            Put_Line(Day08.Part1(Input));
            Put_Line("Part 2:");
            Put_Line(Day08.Part2(Input));
        when 9 => 
            Put_Line("Part 1:");
            Put_Line(Day09.Part1(Input));
            Put_Line("Part 2:");
            Put_Line(Day09.Part2(Input));
        when 10 => 
            Put_Line("Part 1:");
            Put_Line(Day10.Part1(Input));
            Put_Line("Part 2:");
            Put_Line(Day10.Part2(Input));
        when 11 => 
            Put_Line("Part 1:");
            Put_Line(Day11.Part1(Input));
            Put_Line("Part 2:");
            Put_Line(Day11.Part2(Input));
        when 12 => 
            Put_Line("Part 1:");
            Put_Line(Day12.Part1(Input));
            Put_Line("Part 2:");
            Put_Line(Day12.Part2(Input));
        when 13 => 
            Put_Line("Part 1:");
            Put_Line(Day13.Part1(Input));
            Put_Line("Part 2:");
            Put_Line(Day13.Part2(Input));
        when 14 => 
            Put_Line("Part 1:");
            Put_Line(Day14.Part1(Input));
            Put_Line("Part 2:");
            Put_Line(Day14.Part2(Input));
        when 15 => 
            Put_Line("Part 1:");
            Put_Line(Day15.Part1(Input));
            Put_Line("Part 2:");
            Put_Line(Day15.Part2(Input));
        when 16 => 
            Put_Line("Part 1:");
            Put_Line(Day16.Part1(Input));
            Put_Line("Part 2:");
            Put_Line(Day16.Part2(Input));
        when 17 => 
            Put_Line("Part 1:");
            Put_Line(Day17.Part1(Input));
            Put_Line("Part 2:");
            Put_Line(Day17.Part2(Input));
        when 18 => 
            Put_Line("Part 1:");
            Put_Line(Day18.Part1(Input));
            Put_Line("Part 2:");
            Put_Line(Day18.Part2(Input));
        when 19 => 
            Put_Line("Part 1:");
            Put_Line(Day19.Part1(Input));
            Put_Line("Part 2:");
            Put_Line(Day19.Part2(Input));
        when 20 => 
            Put_Line("Part 1:");
            Put_Line(Day20.Part1(Input));
            Put_Line("Part 2:");
            Put_Line(Day20.Part2(Input));
        when 21 => 
            Put_Line("Part 1:");
            Put_Line(Day21.Part1(Input));
            Put_Line("Part 2:");
            Put_Line(Day21.Part2(Input));
        when 22 => 
            Put_Line("Part 1:");
            Put_Line(Day22.Part1(Input));
            Put_Line("Part 2:");
            Put_Line(Day22.Part2(Input));
        when 23 => 
            Put_Line("Part 1:");
            Put_Line(Day23.Part1(Input));
            Put_Line("Part 2:");
            Put_Line(Day23.Part2(Input));
        when 24 => 
            Put_Line("Part 1:");
            Put_Line(Day24.Part1(Input));
            Put_Line("Part 2:");
            Put_Line(Day24.Part2(Input));
        when 25 => 
            Put_Line("Part 1:");
            Put_Line(Day25.Part1(Input));
            Put_Line("Part 2:");
            Put_Line(Day25.Part2(Input));
    end case;
exception
    when E: File_Not_Found_Error =>
        Put_Line(Exception_Message(E) & " (Files must be in the input directory)");
        Ada.Command_Line.Set_Exit_Status(1);
    when E: Program_Error =>
        Put_Line(Exception_Message(E));
        Ada.Command_Line.Set_Exit_Status(1);
end AoC;