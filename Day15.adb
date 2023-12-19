with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Advent_IO; use Advent_IO;

package body Day15 is

function HASH(S: Unbounded_String) return Natural is
    Result: Natural := 0;
begin
    for I in 1..Length(S) loop
        Result := Result + Character'Pos(Element(S, I));
        Result := Result * 17;
        Result := Result rem 256;
    end loop;
    return Result;
end HASH;

function Part1(Input: Puzzle_Input) return String is
    Current_String: Unbounded_String;
    I: Natural;
    Total: Natural := 0;
    Str: Unbounded_String := Input.First_Element;
begin
    I := 1;
    while I <= Length(Str) loop
        if Element(Str, I) = ',' then
            Total := Total + HASH(Current_String);
            Current_String := Null_Unbounded_String;
        else
            Append(Current_String, Element(Str, I));
        end if;
        I := I + 1;
    end loop;
    Total := Total + HASH(Current_String);
    return Total'Image;
end Part1;

function Part2(Input: Puzzle_Input) return String is
    Str: Unbounded_String := Input.First_Element;
    Current_Char: Character;
    type Lens is record
        Label: Unbounded_String;
        Focal_Length: Natural;
    end record;
    function Lens_Eq(L, R: Lens) return Boolean is
    begin
        return L.Label = R.Label;
    end Lens_Eq;
    package Lens_Vec is new Ada.Containers.Vectors
      (Index_Type   => Natural,
       Element_Type => Lens,
       "="          => Lens_Eq);
    use Lens_Vec;
    Map: array(0..255) of Vector;
    Label: Unbounded_String;
    Label_Hash: Natural;
    Expecting_Digit: Boolean := False;
    Focal_Length: Natural;
    Existing: Cursor;
    Temp_Lens, New_Lens: Lens;
    Total: Natural := 0;
begin
    for I in 1..Length(Str) loop
        Current_Char := Element(Str, I);
        case Current_Char is
            when '=' =>
                Expecting_Digit := True;
                Label_Hash := HASH(Label);
            when '-' =>
                Label_Hash := HASH(Label);
                -- create temp lens to find existing lens
                Temp_Lens := (Label => Label, Focal_Length => 0);
                Existing := Map(Label_Hash).Find(Temp_Lens);
                -- if a lens with this label exists then delete it
                if Existing /= No_Element then
                    Map(Label_Hash).Delete(Existing);
                end if;
            when ',' =>
                Label := Null_Unbounded_String;
            when others => 
                if Expecting_Digit then
                    Focal_Length := Character'Pos(Current_Char) - Character'Pos('0');
                    -- create temp lens to find existing lens
                    Temp_Lens := (Label => Label, Focal_Length => 0);
                    Existing := Map(Label_Hash).Find(Temp_Lens);
                    -- if lens with this label exists then update its focal length
                    -- otherwise, create a new lens
                    if Existing /= No_Element then
                        Map(Label_Hash)(Existing).Focal_Length := Focal_Length;
                    else
                        New_Lens := (Label => Label, Focal_Length => Focal_Length);
                        Map(Label_Hash).Append(New_Lens);
                    end if;
                    Expecting_Digit := False;
                else
                    Append(Label, Current_Char);
                end if;
        end case;
    end loop;
    -- evaluate map
    for I in 0..255 loop
        for J in Map(I).First_Index..Map(I).Last_Index loop
            Total := Total + ((I + 1) * (J + 1) * Map(I)(J).Focal_Length);
        end loop;
    end loop;
    return Total'Image;
end Part2;

end Day15;
