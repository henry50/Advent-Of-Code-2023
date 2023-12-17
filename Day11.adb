with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Advent_IO; use Advent_IO;

package body Day11 is

type Part_Enum is (Part_1, Part_2);

function Solve(Input: Puzzle_Input; Part: Part_Enum) return Long_Long_Integer is
    type Row_Idx is new Natural range Input.First_Index..Input.Last_Index;
    type Col_Idx is new Natural range 0..Length(Input(0))-1;
    type Coord is record
        Row: Integer;
        Col: Integer;
    end record;
    package Coord_Vec is new Ada.Containers.Vectors
      (Index_Type   => Natural,
       Element_Type => Coord);
    Galaxy_Location: Coord_Vec.Vector;
    Row_Empty: array(Row_Idx) of Boolean := (others => True);
    Col_Empty: array(Col_Idx) of Boolean := (others => True);
    Line: Unbounded_String;
    Galaxy_1, Galaxy_2: Coord;
    Distance_Increase: Natural := (case Part is
        when Part_1 => 1,
        when Part_2 => 999_999
    );
    Total: Long_Long_Integer := 0;
begin
    -- parse input
    for I in Row_Idx loop
        Line := Input(Natural(I));
        for J in Col_Idx loop
            if Element(Line, Natural(J+1)) = '#' then
                Galaxy_1 := (Row => Natural(I), Col => Natural(J));
                Galaxy_Location.Append(Galaxy_1);
                Row_Empty(I) := False;
                Col_Empty(J) := False;
            end if;
        end loop;
    end loop;
    -- add spaces
    for Galaxy of Galaxy_Location loop
        for I in 0..Galaxy.Row loop
            if Row_Empty(Row_Idx(I)) then
                Galaxy.Row := Galaxy.Row + Distance_Increase;
            end if;
        end loop;
        for J in 0..Galaxy.Col loop
            if Col_Empty(Col_Idx(J)) then
                Galaxy.Col := Galaxy.Col + Distance_Increase;
            end if;
        end loop;
    end loop;
    -- find distances
    for I in Galaxy_Location.First_Index..Galaxy_Location.Last_Index-1 loop
        for J in I+1..Galaxy_Location.Last_Index loop
            Galaxy_1 := Galaxy_Location(I);
            Galaxy_2 := Galaxy_Location(J);
            Total := Total + Long_Long_Integer(
                abs(Galaxy_1.Row - Galaxy_2.Row) + abs(Galaxy_1.Col - Galaxy_2.Col)
            );
        end loop;
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

end Day11;
