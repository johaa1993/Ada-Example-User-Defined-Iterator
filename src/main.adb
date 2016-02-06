with Ada.Iterator_Interfaces;
with Ada.Text_IO;

procedure Main is

   package Stacks is
      type Stack (Count : Natural) is private;
      type Stack_Access is access all Stack;
      type Cursor is private;
      function Has_Element (Position : Cursor) return Boolean;
      package Iterators is new Ada.Iterator_Interfaces (Cursor, Has_Element);
      type Iterator is new Iterators.Forward_Iterator with private;
      overriding function First (Item : Iterator) return Cursor;
      overriding function Next (Item : Iterator; Position : Cursor) return Cursor;
      procedure Push (Item : Integer; Destination : in out Stack);
      function Iterate (Item : Stack) return Iterators.Forward_Iterator'Class;
      function Element (Position : Cursor) return Integer;
   private
      type Integer_Array is array (Integer range <>) of Integer;

      type Stack (Count : Natural) is record
         List : Integer_Array (1 .. Count);
         Top : Natural := 0;
      end record;

      type Iterator is new Iterators.Forward_Iterator with record
         Component : Stack_Access;
      end record;

      type Cursor is record
         Component : Stack_Access;
         Index : Positive := 1;
      end record;
   end;




   package body Stacks is

      procedure Push (Item : Integer; Destination : in out Stack) is
      begin
         Destination.Top := Destination.Top + 1;
         Destination.List (Destination.Top) := Item;
      end;

      function Has_Element (Position : Cursor) return Boolean is
      begin
         return Position.Index <= Position.Component.Top;
      end;

      function First (Item : Iterator) return Cursor is
      begin
         return Position : Cursor do
            Position.Component := Item.Component;
            Position.Index := 1;
         end return;
      end;

      function Next (Item : Iterator; Position : Cursor) return Cursor is
         pragma Unreferenced (Item);
      begin
         return C : Cursor do
            C.Component := Position.Component;
            C.Index := Position.Index + 1;
         end return;
      end;

      function Iterate (Item : Stack) return Iterators.Forward_Iterator'Class is
      begin
         return I : Iterator do
            I.Component := Item'Unrestricted_Access;
         end return;
      end;

      function Element (Position : Cursor) return Integer is
      begin
         return Position.Component.List (Position.Index);
      end;

   end;

   use Stacks;
   use Ada.Text_IO;

   S : Stack (10);

begin

   Push (3, S);
   Push (6, S);
   Push (8, S);

   for C in Iterate (S) loop
      Put_Line (Element (C)'Img);
   end loop;

end;
