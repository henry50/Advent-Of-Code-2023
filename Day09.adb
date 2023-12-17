with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Advent_IO; use Advent_IO;

package body Day09 is

package Integer_Vec is new Ada.Containers.Vectors
  (Index_Type   => Natural,
   Element_Type => Integer);

package Integer_Vec_Vec is new Ada.Containers.Vectors
  (Index_Type   => Natural,
   Element_Type => Integer_Vec.Vector,
   "="          => Integer_Vec."=");

type Part_Enum is (Part_1, Part_2);

function Is_Digit(C: Character) return Boolean is
begin
    return Character'Pos(C) - Character'Pos('0') in 0..9;
end Is_Digit;

function Parse_Input(Input: Puzzle_Input) return Integer_Vec_Vec.Vector is
    Parsed: Integer_Vec_Vec.Vector;
    Num_Str: Unbounded_String;
    Current_Seq: Integer_Vec.Vector;
    I: Natural;
begin
    for Line of Input loop
        I := 1;
        while I <= Length(Line) loop
            Num_Str := Null_Unbounded_String;
            loop
                Append(Num_Str, Element(Line, I));
                I := I + 1;
                exit when I > Length(Line);
                exit when Element(Line, I) = ' ';
            end loop;
            Current_Seq.Append(Integer'Value(To_String(Num_Str)));
            -- consume space
            I := I + 1;
        end loop;
        Parsed.Append(Current_Seq);
        Current_Seq.Clear;
    end loop;
    return Parsed;
end Parse_Input;

function Solve(Input: Puzzle_Input; Part: Part_Enum) return Integer is
    Parsed: Integer_Vec_Vec.Vector := Parse_Input(Input);
    Sequences: Integer_Vec_Vec.Vector;
    Total: Integer := 0;
    Current: Integer_Vec.Vector;
    Temp: Integer_Vec.Vector;
    Extrapolated: Integer;
    function Is_Zero(V: Integer_Vec.Vector) return Boolean is
    begin
        for I of V loop
            if I /= 0 then
                return False;
            end if;
        end loop;
        return True;
    end Is_Zero;
begin
    for History of Parsed loop
        Current := History;
        Sequences.Append(History);
        -- get differences until all zero
        while not Is_Zero(Current) loop
            for I in Current.First_Index..Current.Last_Index-1 loop
                Temp.Append(Current(I+1) - Current(I));
            end loop;
            Current := Temp;
            Sequences.Append(Current);
            Temp.Clear;
        end loop;
        -- extrapolate values
        Extrapolated := 0;
        for I in reverse Sequences.First_Index..Sequences.Last_Index loop
            Extrapolated := (case Part is
                when Part_1 => Extrapolated + Sequences(I).Last_Element,
                when Part_2 => Sequences(I).First_Element - Extrapolated
            );
        end loop;
        -- calculate total
        Total := Total + Extrapolated;
        Sequences.Clear;
    end loop;
    return Total;
end Solve;

function Part1(Input: Puzzle_Input) return String is
begin
    return Solve(Input, Part_1)'Image;
end Part1;

function Part2(Input: Puzzle_Input) return String is
begin
    return Solve(Input, Part_2)'Image;
end Part2;

end Day09;
