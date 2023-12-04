with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Sets;
with Advent_IO; use Advent_IO;

package body Day03 is

type Engine_Num is record
    Value: Integer;
    Id: Natural;
end record;
package Row_Vec is new Ada.Containers.Vectors
    (Index_Type  => Natural,
    Element_Type => Engine_Num);
package Engine_Vec is new Ada.Containers.Vectors
    (Index_Type  => Natural,
    Element_Type => Row_Vec.Vector,
    "=" => Row_Vec."=");


function Is_Digit(C: Character) return Boolean is
begin
    return Character'Pos(C) - Character'Pos('0') in 0..9;
end Is_Digit;

-- This function produces a map like the following for the example input
-- [
--  [467, 467, 467, -1, -1, 114, 114, 114, -1 , -1],
--  [-1 , -1 , -1 , -1, -1, -1 , -1 , -1 , -1 , -1],
--  [-1 , -1 , 35 , 35, -1, -1 , 663, 663, 663, -1]
--  and so on
-- ]
-- Then when a symbol is found in the input, the adjacent numbers can be
-- found easily. To prevent adding the same number multiple times, each number
-- has an ID. For example, all of the 467 above would have the same ID because
-- they represent the same number in the input. -1 represents the lack of a number
-- and always has ID 0.
function Parse_Input(Input: Puzzle_Input) return Engine_Vec.Vector is
    I: Natural;
    Result: Engine_Vec.Vector;
    Current_Row: Row_Vec.Vector;
    type Len_Num is record
        Value: Natural;
        Len: Natural;
    end record;
    Num: Len_Num;
    -- This uniquely identifies groups of the same number
    Num_Id: Natural := 1;
    Eng_Num: Engine_Num;
    function Read_Num(S: Unbounded_String) return Len_Num is
        Result: Unbounded_String;
    begin
        loop
            Append(Result, Element(S, I));
            I := I + 1;
            exit when I > Length(S);
            exit when not Is_Digit(Element(S, I));
        end loop;
        return (
            Value  => Natural'Value(To_String(Result)),
            Len => Length(Result)
        );
    end Read_Num; 
begin
    for Line of Input loop
        Current_Row.Clear;
        I := 1;
        loop
            if Is_Digit(Element(Line, I)) then
                Num := Read_Num(Line);
                for J in 1..Num.Len loop
                    Eng_Num := (Value => Num.Value, Id => Num_Id);
                    Current_Row.Append(Eng_Num);
                end loop;
                Num_Id := Num_Id + 1;
            else
                I := I + 1;
                Eng_Num := (Value => -1, Id => 0);
                Current_Row.Append(Eng_Num);
            end if;
            exit when I > Length(Line);
        end loop;
        Result.Append(Current_Row);
    end loop;
    return Result;
end;

-- Iterates every row and column of the Parse_Input map looking for
-- symbols and their adjacent values
function Common(Input: Puzzle_Input; Part: Natural) return Integer is
    Engine: Engine_Vec.Vector := Parse_Input(Input);
    Current_Row: Row_Vec.Vector;
    Current_Line: Unbounded_String;
    Current_Char: Character;
    Current_Num: Engine_Num;
    package Adjacent_Set is new Ada.Containers.Ordered_Sets(Element_Type => Natural);
    package Adjacent_Vec is new Ada.Containers.Vectors(Index_Type => Natural, Element_Type => Natural);
    Adjacent_Ids: Adjacent_Set.Set;
    Adjacent_Vals: Adjacent_Vec.Vector;
    Total: Natural := 0;
begin
    -- For each row (0-indexed)
    for I in Input.First_Index..Input.Last_Index loop
        Current_Row := Engine.Element(I);
        Current_Line := Input.Element(I);
        -- For each column (1-indexed)
        for J in 1..Length(Input(I)) loop
            Current_Char := Element(Current_Line, J);
            Adjacent_Ids.Clear;
            Adjacent_Vals.Clear;
            if (Part = 1 and (not (Is_Digit(Current_Char) or Current_Char = '.')))
            or (Part = 2 and Current_Char = '*') then
                -- symbol encountered, check adjacent for numbers
                for X in -1..1 loop
                    for Y in -1..1 loop
                        -- Get the current number at that position
                        Current_Num := Engine.Element(I+X).Element(J+Y-1);
                        -- If the number is valid and not already included, add it to adjacent
                        if Current_Num.Value /= -1 and not Adjacent_Ids.Contains(Current_Num.Id) then
                            if Part = 1 then
                                Total := Total + Current_Num.Value;
                            end if;
                            Adjacent_Vals.Append(Current_Num.Value);
                            Adjacent_Ids.Insert(Current_Num.Id);
                        end if;
                    end loop;
                end loop;
                if Part = 2 and Natural(Adjacent_Vals.Length) = 2 then
                    Total := Total + (Adjacent_Vals.Element(0) * Adjacent_Vals.Element(1));
                end if;
            end if;
        end loop;
    end loop;
    return Total;
end Common;

function Part1(Input: Puzzle_Input) return Integer is
begin
    return Common(Input, 1);
end Part1;

function Part2(Input: Puzzle_Input) return Integer is
begin
    return Common(Input, 2);
end Part2;

end Day03;
