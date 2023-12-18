with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Advent_IO; use Advent_IO;

package body Day13 is

package Natural_Vec is new Ada.Containers.Vectors
  (Index_Type   => Natural,
   Element_Type => Natural);

type Pattern is record
    Rows: Natural_Vec.Vector;
    Cols: Natural_Vec.Vector;
end record;

package Pattern_Vec is new Ada.Containers.Vectors
  (Index_Type => Natural,
   Element_Type => Pattern);

function Parse_Input(Input: Puzzle_Input) return Pattern_Vec.Vector is
    Current_Pattern: Unbounded_String_Vector.Vector;
    Result: Pattern_Vec.Vector;
    function Parse_Pattern(Pattern_Str: Unbounded_String_Vector.Vector) return Pattern is
        Parsed_Pattern: Pattern;
        type Row_Idx is new Natural range Pattern_Str.First_Index..Pattern_Str.Last_Index;
        type Col_Idx is new Natural range 0..Length(Pattern_Str.First_Element)-1;
        Current_Number: Natural;
    begin
        -- convert pattern to binary to number
        for I in Row_Idx loop
            Current_Number := 0;
            for J in Col_Idx loop
                Current_Number := (Current_Number * 2) + (case Element(Pattern_Str(Natural(I)), Natural(J)+1) is
                    when '#' => 1,
                    when others => 0
                );
            end loop;
            Parsed_Pattern.Rows.Append(Current_Number);
        end loop;
        for J in Col_Idx loop
            Current_Number := 0;
            for I in Row_Idx loop
                Current_Number := (Current_Number * 2) + (case Element(Pattern_Str(Natural(I)), Natural(J)+1) is
                    when '#' => 1,
                    when others => 0
                );
            end loop;
            Parsed_Pattern.Cols.Append(Current_Number);
        end loop;
        return Parsed_Pattern;
    end Parse_Pattern;
begin
    for Line of Input loop
        if Length(Line) = 0 then
            Result.Append(Parse_Pattern(Current_Pattern));
            Current_Pattern.Clear;
        else
            Current_Pattern.Append(Line);
        end if;
    end loop;
    Result.Append(Parse_Pattern(Current_Pattern));
    return Result;
end Parse_Input;

function Check_Reflection(V: Natural_Vec.Vector; Ingore_First: Boolean := False) return Natural is
    Width: Natural;
    -- a hacky way to fix part 2
    Already_Ignored: Boolean := False;
    -- check if the left side of the midpoint is equal to the
    -- reverse of the right side
    function Check_Equal(Start, Mid: Natural) return Boolean is
    begin
        for I in Start..Mid-1 loop
            if V(I) /= V((2*Mid) - I - 1) then
                return False;
            end if;
        end loop;
        return True;
    end Check_Equal;
begin
    -- consider every line of reflection from left to right/top to bottom
    for Midpoint in 1..V.Last_Index loop
        Width := Natural'Min(Midpoint, V.Last_Index + 1 - Midpoint);
        if Check_Equal(Midpoint-Width, Midpoint) then
            -- for part 2, optionally ignore the first
            -- result to get a new second result
            if not Ingore_First or Already_Ignored then
                return Midpoint;
            else
                Already_Ignored := True;
            end if;
        end if;
    end loop;
    return 0;
end Check_Reflection;

function Part1(Input: Puzzle_Input) return String is
    Parsed: Pattern_Vec.Vector := Parse_Input(Input);
    Total: Natural := 0;
    function Summarise_Pattern(P: Pattern) return Natural is
        Result: Natural;
        Cannot_Summarise: exception;
    begin
        Result := Check_Reflection(P.Cols);
        if Result /= 0 then
            return Result;
        end if;
        Result := 100 * Check_Reflection(P.Rows);
        if Result /= 0 then
            return Result;
        end if;
        raise Cannot_Summarise;
    end Summarise_Pattern;
begin
    for P of Parsed loop
        Total := Total + Summarise_Pattern(P);
    end loop;
    return Total'Image;
end Part1;

function Part2(Input: Puzzle_Input) return String is
    Parsed: Pattern_Vec.Vector := Parse_Input(Input);
    Result, Total: Natural := 0;
    function Unsmudge_Pattern(P: Pattern) return Natural is
        Result: Natural;
        function Unsmudge(F, S: Natural_Vec.Vector; Old_Reflection: Natural) return Natural is
            -- modular types have to be used for xor
            type Mod_Natural is mod 2**31-1;
            Flip: Natural_Vec.Vector := F;
            Shift: Natural_Vec.Vector := S;
            New_P: Pattern;
            Prev: Natural;
            Current_Reflection: Natural;
        begin
            for I in Flip.First_Index..Flip.Last_Index loop
                for J in Shift.First_Index..Shift.Last_Index loop
                    Prev := Flip(I);
                    -- try bit flipping the Jth bit
                    Flip(I) := Natural(Mod_Natural(Prev) xor Mod_Natural(2**J));
                    Current_Reflection := Check_Reflection(Flip);
                    -- if the new reflection is the same, try ignoring the first result
                    if Current_Reflection = Old_Reflection then
                        Current_Reflection := Check_Reflection(Flip, True);
                    end if;
                    -- check if reflection has changed
                    if Current_Reflection /= 0 and Current_Reflection /= Old_Reflection then
                        return Current_Reflection;
                    end if;
                    Flip(I) := Prev;
                end loop;
            end loop;
            return 0;
        end Unsmudge;
        Cannot_Unsmudge: exception;
    begin
        Result := Unsmudge(P.Rows, P.Cols, Check_Reflection(P.Rows));
        if Result /= 0 then
            return 100 * Result;
        end if;
        Result := Unsmudge(P.Cols, P.Rows, Check_Reflection(P.Cols));
        if Result /= 0 then
            return Result;
        end if;
        raise Cannot_Unsmudge;
    end Unsmudge_Pattern;
begin
    for P of Parsed loop
        Total := Total + Unsmudge_Pattern(P);
    end loop;
    return Total'Image;
end Part2;

end Day13;
