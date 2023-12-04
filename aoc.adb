with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions; use Ada.Exceptions;

with Advent_IO; use Advent_IO;

with Day01; with Day02; with Day03; with Day04; with Day05;
with Day06; with Day07; with Day08; with Day09; with Day10;
with Day11; with Day12; with Day13; with Day14; with Day15;
with Day16; with Day17; with Day18; with Day19; with Day20;
with Day21; with Day22; with Day23; with Day24; with Day25;

procedure AoC is
    -- Get this from cmd line args
    Day_To_Run: Day := 2;
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
        when 2 => 
            Put_Line("Part 1:");
            Put(Day02.Part1(Input)); New_Line;
            Put_Line("Part 2:");
            Put(Day02.Part2(Input)); New_Line;
        when 3 => 
            Put_Line("Part 1:");
            Put(Day03.Part1(Input)); New_Line;
            Put_Line("Part 2:");
            Put(Day03.Part2(Input)); New_Line;
        when 4 => 
            Put_Line("Part 1:");
            Put(Day04.Part1(Input)); New_Line;
            Put_Line("Part 2:");
            Put(Day04.Part2(Input)); New_Line;
        when 5 => 
            Put_Line("Part 1:");
            Put(Day05.Part1(Input)); New_Line;
            Put_Line("Part 2:");
            Put(Day05.Part2(Input)); New_Line;
        when 6 => 
            Put_Line("Part 1:");
            Put(Day06.Part1(Input)); New_Line;
            Put_Line("Part 2:");
            Put(Day06.Part2(Input)); New_Line;
        when 7 => 
            Put_Line("Part 1:");
            Put(Day07.Part1(Input)); New_Line;
            Put_Line("Part 2:");
            Put(Day07.Part2(Input)); New_Line;
        when 8 => 
            Put_Line("Part 1:");
            Put(Day08.Part1(Input)); New_Line;
            Put_Line("Part 2:");
            Put(Day08.Part2(Input)); New_Line;
        when 9 => 
            Put_Line("Part 1:");
            Put(Day09.Part1(Input)); New_Line;
            Put_Line("Part 2:");
            Put(Day09.Part2(Input)); New_Line;
        when 10 => 
            Put_Line("Part 1:");
            Put(Day10.Part1(Input)); New_Line;
            Put_Line("Part 2:");
            Put(Day10.Part2(Input)); New_Line;
        when 11 => 
            Put_Line("Part 1:");
            Put(Day11.Part1(Input)); New_Line;
            Put_Line("Part 2:");
            Put(Day11.Part2(Input)); New_Line;
        when 12 => 
            Put_Line("Part 1:");
            Put(Day12.Part1(Input)); New_Line;
            Put_Line("Part 2:");
            Put(Day12.Part2(Input)); New_Line;
        when 13 => 
            Put_Line("Part 1:");
            Put(Day13.Part1(Input)); New_Line;
            Put_Line("Part 2:");
            Put(Day13.Part2(Input)); New_Line;
        when 14 => 
            Put_Line("Part 1:");
            Put(Day14.Part1(Input)); New_Line;
            Put_Line("Part 2:");
            Put(Day14.Part2(Input)); New_Line;
        when 15 => 
            Put_Line("Part 1:");
            Put(Day15.Part1(Input)); New_Line;
            Put_Line("Part 2:");
            Put(Day15.Part2(Input)); New_Line;
        when 16 => 
            Put_Line("Part 1:");
            Put(Day16.Part1(Input)); New_Line;
            Put_Line("Part 2:");
            Put(Day16.Part2(Input)); New_Line;
        when 17 => 
            Put_Line("Part 1:");
            Put(Day17.Part1(Input)); New_Line;
            Put_Line("Part 2:");
            Put(Day17.Part2(Input)); New_Line;
        when 18 => 
            Put_Line("Part 1:");
            Put(Day18.Part1(Input)); New_Line;
            Put_Line("Part 2:");
            Put(Day18.Part2(Input)); New_Line;
        when 19 => 
            Put_Line("Part 1:");
            Put(Day19.Part1(Input)); New_Line;
            Put_Line("Part 2:");
            Put(Day19.Part2(Input)); New_Line;
        when 20 => 
            Put_Line("Part 1:");
            Put(Day20.Part1(Input)); New_Line;
            Put_Line("Part 2:");
            Put(Day20.Part2(Input)); New_Line;
        when 21 => 
            Put_Line("Part 1:");
            Put(Day21.Part1(Input)); New_Line;
            Put_Line("Part 2:");
            Put(Day21.Part2(Input)); New_Line;
        when 22 => 
            Put_Line("Part 1:");
            Put(Day22.Part1(Input)); New_Line;
            Put_Line("Part 2:");
            Put(Day22.Part2(Input)); New_Line;
        when 23 => 
            Put_Line("Part 1:");
            Put(Day23.Part1(Input)); New_Line;
            Put_Line("Part 2:");
            Put(Day23.Part2(Input)); New_Line;
        when 24 => 
            Put_Line("Part 1:");
            Put(Day24.Part1(Input)); New_Line;
            Put_Line("Part 2:");
            Put(Day24.Part2(Input)); New_Line;
        when 25 => 
            Put_Line("Part 1:");
            Put(Day25.Part1(Input)); New_Line;
            Put_Line("Part 2:");
            Put(Day25.Part2(Input)); New_Line;
    end case;
exception
    when E: File_Not_Found_Error =>
        Put_Line("Unable to open input file: " & Exception_Message(E));
end AoC;