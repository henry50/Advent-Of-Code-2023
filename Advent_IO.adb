with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Directories; use Ada.Directories;

package body Advent_IO is

function Pad_Day(D: Day; Example: Boolean) return String is
    Day_Image: String := D'Image;
    File_Prefix: Unbounded_String := To_Unbounded_String("day");
begin
    if Example then
        File_Prefix := To_Unbounded_String("ex");
    end if;
    if D < 10 then
        return To_String(File_Prefix) & "0" & Day_Image(2);
    else
        return To_String(File_Prefix) & Day_Image(2..3);
    end if;
end Pad_Day;

function Get_Input(D: Day; Example: Boolean) return Puzzle_Input is
    Filename: String := "./input/" & Pad_Day(D, Example) & ".txt";
    File: File_Type;
    Line: Unbounded_String;
    Output: Puzzle_Input;
begin
    if not Exists(Filename) then
        raise File_Not_Found_Error with "Could not open input file: " & Filename & " does not exist";
    end if;
    Open(File, In_File, Filename);
    loop
        exit when End_Of_File(File);
        Get_Line(File, Line);
        Output.Append(Line);
    end loop;
    return Output;
end Get_Input;

end Advent_IO;