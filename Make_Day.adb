with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

procedure Make_Day is
    ADS_1: String := "with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;" & LF
                   & "with Advent_IO; use Advent_IO;" & LF
                   & "package Day";
    ADS_2: String := " is" & LF
                   & LF
                   & "function Part1(Input: Puzzle_Input) return String;" & LF
                   & "function Part2(Input: Puzzle_Input) return String;" & LF
                   & LF
                   & "end Day";
    ADS_3: String := ";";
    ADB_1: String := "with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;" & LF
                   & "with Advent_IO; use Advent_IO;" & LF
                   & LF
                   & "package body Day";
    ADB_2: String := " is" & LF
                     & LF
                     & "function Part1(Input: Puzzle_Input) return String is" & LF
                     & "    " & LF
                     & "begin" & LF
                     & "    return 0;" & LF
                     & "end Part1;" & LF
                     & LF
                     & "function Part2(Input: Puzzle_Input) return String is" & LF
                     & "    " & LF
                     & "begin" & LF
                     & "    return 0;" & LF
                     & "end Part2;" & LF
                     & LF
                     & "end Day";
    ADB_3: String := ";";
    DAY_1: String := "        when ";
    DAY_2: String := " => " & LF
                   & "            Put_Line(""Part 1:"");" & LF
                   & "            Put(Day";
    DAY_3: String := ".Part1(Input)); New_Line;" & LF
                   & "            Put_Line(""Part 2:"");" & LF
                   & "            Put(Day";
    DAY_4: String := ".Part2(Input)); New_Line;" & LF;
    function N(D: Natural) return String is
        Day_Image: String := D'Image;
    begin
        if D < 10 then
            return "0" & Day_Image(2);
        else
            return Day_Image(2..3);
        end if;
    end N;
    procedure Make_Files is
        File: File_Type;
    begin
        for I in 1..25 loop
            Create(File, Out_File, "Day" & N(I) & ".ads");
            Put(File, ADS_1 & N(I) & ADS_2 & N(I) & ADS_3);
            Close(File);
            Create(File, Out_File, "Day" & N(I) & ".adb");
            Put(File, ADB_1 & N(I) & ADB_2 & N(I) & ADB_3);
            Close(File);
        end loop;
    end Make_Files;
    procedure Make_Case is
        File: File_Type;
    begin
        Create(File, Out_File, "aoc_cases.txt");
        for I in 1..25 loop
            Put(File, DAY_1 & N(I) & DAY_2 & N(I) & DAY_3 & N(I) & DAY_4);
        end loop;
        Close(File);
    end Make_Case;
begin
    -- Make_Files;
    -- Make_Case;
    null;
end Make_Day;