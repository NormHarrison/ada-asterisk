pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__test.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__test.adb");
pragma Suppress (Overflow_Check);

with System.Restrictions;
with Ada.Exceptions;

package body ada_main is

   E073 : Short_Integer; pragma Import (Ada, E073, "system__os_lib_E");
   E005 : Short_Integer; pragma Import (Ada, E005, "ada__exceptions_E");
   E011 : Short_Integer; pragma Import (Ada, E011, "system__soft_links_E");
   E023 : Short_Integer; pragma Import (Ada, E023, "system__exception_table_E");
   E038 : Short_Integer; pragma Import (Ada, E038, "ada__containers_E");
   E068 : Short_Integer; pragma Import (Ada, E068, "ada__io_exceptions_E");
   E053 : Short_Integer; pragma Import (Ada, E053, "ada__strings_E");
   E055 : Short_Integer; pragma Import (Ada, E055, "ada__strings__maps_E");
   E059 : Short_Integer; pragma Import (Ada, E059, "ada__strings__maps__constants_E");
   E043 : Short_Integer; pragma Import (Ada, E043, "interfaces__c_E");
   E025 : Short_Integer; pragma Import (Ada, E025, "system__exceptions_E");
   E079 : Short_Integer; pragma Import (Ada, E079, "system__object_reader_E");
   E048 : Short_Integer; pragma Import (Ada, E048, "system__dwarf_lines_E");
   E019 : Short_Integer; pragma Import (Ada, E019, "system__soft_links__initialize_E");
   E037 : Short_Integer; pragma Import (Ada, E037, "system__traceback__symbolic_E");
   E097 : Short_Integer; pragma Import (Ada, E097, "ada__tags_E");
   E149 : Short_Integer; pragma Import (Ada, E149, "ada__streams_E");
   E205 : Short_Integer; pragma Import (Ada, E205, "gnat_E");
   E218 : Short_Integer; pragma Import (Ada, E218, "interfaces__c__strings_E");
   E151 : Short_Integer; pragma Import (Ada, E151, "system__finalization_root_E");
   E147 : Short_Integer; pragma Import (Ada, E147, "ada__finalization_E");
   E177 : Short_Integer; pragma Import (Ada, E177, "system__storage_pools_E");
   E173 : Short_Integer; pragma Import (Ada, E173, "system__finalization_masters_E");
   E171 : Short_Integer; pragma Import (Ada, E171, "system__storage_pools__subpools_E");
   E163 : Short_Integer; pragma Import (Ada, E163, "ada__strings__unbounded_E");
   E124 : Short_Integer; pragma Import (Ada, E124, "system__task_info_E");
   E116 : Short_Integer; pragma Import (Ada, E116, "system__task_primitives__operations_E");
   E195 : Short_Integer; pragma Import (Ada, E195, "ada__calendar_E");
   E193 : Short_Integer; pragma Import (Ada, E193, "ada__calendar__delays_E");
   E242 : Short_Integer; pragma Import (Ada, E242, "ada__real_time_E");
   E246 : Short_Integer; pragma Import (Ada, E246, "system__interrupt_management__operations_E");
   E200 : Short_Integer; pragma Import (Ada, E200, "system__pool_global_E");
   E222 : Short_Integer; pragma Import (Ada, E222, "system__pool_size_E");
   E207 : Short_Integer; pragma Import (Ada, E207, "gnat__sockets_E");
   E216 : Short_Integer; pragma Import (Ada, E216, "gnat__sockets__thin_common_E");
   E210 : Short_Integer; pragma Import (Ada, E210, "gnat__sockets__thin_E");
   E144 : Short_Integer; pragma Import (Ada, E144, "system__tasking__initialization_E");
   E105 : Short_Integer; pragma Import (Ada, E105, "system__tasking__protected_objects_E");
   E140 : Short_Integer; pragma Import (Ada, E140, "system__tasking__protected_objects__entries_E");
   E157 : Short_Integer; pragma Import (Ada, E157, "system__tasking__queuing_E");
   E248 : Short_Integer; pragma Import (Ada, E248, "system__tasking__stages_E");
   E240 : Short_Integer; pragma Import (Ada, E240, "system__tasking__async_delays_E");
   E198 : Short_Integer; pragma Import (Ada, E198, "agnostic_io_E");
   E103 : Short_Integer; pragma Import (Ada, E103, "asterisk_E");
   E204 : Short_Integer; pragma Import (Ada, E204, "socket_aio_E");
   E191 : Short_Integer; pragma Import (Ada, E191, "asterisk__ami_E");

   Sec_Default_Sized_Stacks : array (1 .. 1) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      E191 := E191 - 1;
      declare
         procedure F1;
         pragma Import (Ada, F1, "asterisk__ami__finalize_spec");
      begin
         F1;
      end;
      E204 := E204 - 1;
      declare
         procedure F2;
         pragma Import (Ada, F2, "socket_aio__finalize_spec");
      begin
         F2;
      end;
      declare
         procedure F3;
         pragma Import (Ada, F3, "agnostic_io__finalize_spec");
      begin
         E198 := E198 - 1;
         F3;
      end;
      E140 := E140 - 1;
      declare
         procedure F4;
         pragma Import (Ada, F4, "system__tasking__protected_objects__entries__finalize_spec");
      begin
         F4;
      end;
      declare
         procedure F5;
         pragma Import (Ada, F5, "gnat__sockets__finalize_body");
      begin
         E207 := E207 - 1;
         F5;
      end;
      declare
         procedure F6;
         pragma Import (Ada, F6, "gnat__sockets__finalize_spec");
      begin
         F6;
      end;
      E222 := E222 - 1;
      declare
         procedure F7;
         pragma Import (Ada, F7, "system__pool_size__finalize_spec");
      begin
         F7;
      end;
      E200 := E200 - 1;
      declare
         procedure F8;
         pragma Import (Ada, F8, "system__pool_global__finalize_spec");
      begin
         F8;
      end;
      E163 := E163 - 1;
      declare
         procedure F9;
         pragma Import (Ada, F9, "ada__strings__unbounded__finalize_spec");
      begin
         F9;
      end;
      E171 := E171 - 1;
      declare
         procedure F10;
         pragma Import (Ada, F10, "system__storage_pools__subpools__finalize_spec");
      begin
         F10;
      end;
      E173 := E173 - 1;
      declare
         procedure F11;
         pragma Import (Ada, F11, "system__finalization_masters__finalize_spec");
      begin
         F11;
      end;
      declare
         procedure Reraise_Library_Exception_If_Any;
            pragma Import (Ada, Reraise_Library_Exception_If_Any, "__gnat_reraise_library_exception_if_any");
      begin
         Reraise_Library_Exception_If_Any;
      end;
   end finalize_library;

   procedure adafinal is
      procedure s_stalib_adafinal;
      pragma Import (Ada, s_stalib_adafinal, "system__standard_library__adafinal");

      procedure Runtime_Finalize;
      pragma Import (C, Runtime_Finalize, "__gnat_runtime_finalize");

   begin
      if not Is_Elaborated then
         return;
      end if;
      Is_Elaborated := False;
      Runtime_Finalize;
      s_stalib_adafinal;
   end adafinal;

   type No_Param_Proc is access procedure;
   pragma Favor_Top_Level (No_Param_Proc);

   procedure adainit is
      Main_Priority : Integer;
      pragma Import (C, Main_Priority, "__gl_main_priority");
      Time_Slice_Value : Integer;
      pragma Import (C, Time_Slice_Value, "__gl_time_slice_val");
      WC_Encoding : Character;
      pragma Import (C, WC_Encoding, "__gl_wc_encoding");
      Locking_Policy : Character;
      pragma Import (C, Locking_Policy, "__gl_locking_policy");
      Queuing_Policy : Character;
      pragma Import (C, Queuing_Policy, "__gl_queuing_policy");
      Task_Dispatching_Policy : Character;
      pragma Import (C, Task_Dispatching_Policy, "__gl_task_dispatching_policy");
      Priority_Specific_Dispatching : System.Address;
      pragma Import (C, Priority_Specific_Dispatching, "__gl_priority_specific_dispatching");
      Num_Specific_Dispatching : Integer;
      pragma Import (C, Num_Specific_Dispatching, "__gl_num_specific_dispatching");
      Main_CPU : Integer;
      pragma Import (C, Main_CPU, "__gl_main_cpu");
      Interrupt_States : System.Address;
      pragma Import (C, Interrupt_States, "__gl_interrupt_states");
      Num_Interrupt_States : Integer;
      pragma Import (C, Num_Interrupt_States, "__gl_num_interrupt_states");
      Unreserve_All_Interrupts : Integer;
      pragma Import (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");
      Exception_Tracebacks : Integer;
      pragma Import (C, Exception_Tracebacks, "__gl_exception_tracebacks");
      Detect_Blocking : Integer;
      pragma Import (C, Detect_Blocking, "__gl_detect_blocking");
      Default_Stack_Size : Integer;
      pragma Import (C, Default_Stack_Size, "__gl_default_stack_size");
      Default_Secondary_Stack_Size : System.Parameters.Size_Type;
      pragma Import (C, Default_Secondary_Stack_Size, "__gnat_default_ss_size");
      Bind_Env_Addr : System.Address;
      pragma Import (C, Bind_Env_Addr, "__gl_bind_env_addr");

      procedure Runtime_Initialize (Install_Handler : Integer);
      pragma Import (C, Runtime_Initialize, "__gnat_runtime_initialize");

      Finalize_Library_Objects : No_Param_Proc;
      pragma Import (C, Finalize_Library_Objects, "__gnat_finalize_library_objects");
      Binder_Sec_Stacks_Count : Natural;
      pragma Import (Ada, Binder_Sec_Stacks_Count, "__gnat_binder_ss_count");
      Default_Sized_SS_Pool : System.Address;
      pragma Import (Ada, Default_Sized_SS_Pool, "__gnat_default_ss_pool");

   begin
      if Is_Elaborated then
         return;
      end if;
      Is_Elaborated := True;
      Main_Priority := -1;
      Time_Slice_Value := -1;
      WC_Encoding := 'b';
      Locking_Policy := ' ';
      Queuing_Policy := ' ';
      Task_Dispatching_Policy := ' ';
      System.Restrictions.Run_Time_Restrictions :=
        (Set =>
          (False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False, False, True, False, False, 
           False, False, False, False, False, False, False, False, 
           False, False, False, False),
         Value => (0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         Violated =>
          (True, False, False, False, True, True, False, False, 
           False, False, False, True, True, True, True, False, 
           False, False, False, False, True, True, False, True, 
           True, False, True, True, False, True, False, True, 
           False, False, False, True, False, True, True, False, 
           True, False, True, True, False, True, False, True, 
           True, False, False, True, False, False, False, False, 
           True, False, False, True, False, True, True, True, 
           False, False, True, False, True, True, True, False, 
           True, True, False, True, True, True, True, False, 
           True, False, False, False, False, True, False, False, 
           True, True, True, False),
         Count => (0, 0, 0, 1, 0, 0, 1, 1, 1, 0),
         Unknown => (False, False, False, False, False, False, False, True, True, False));
      Priority_Specific_Dispatching :=
        Local_Priority_Specific_Dispatching'Address;
      Num_Specific_Dispatching := 0;
      Main_CPU := -1;
      Interrupt_States := Local_Interrupt_States'Address;
      Num_Interrupt_States := 0;
      Unreserve_All_Interrupts := 0;
      Exception_Tracebacks := 1;
      Detect_Blocking := 0;
      Default_Stack_Size := -1;

      ada_main'Elab_Body;
      Default_Secondary_Stack_Size := System.Parameters.Runtime_Default_Sec_Stack_Size;
      Binder_Sec_Stacks_Count := 1;
      Default_Sized_SS_Pool := Sec_Default_Sized_Stacks'Address;

      Runtime_Initialize (1);

      Finalize_Library_Objects := finalize_library'access;

      Ada.Exceptions'Elab_Spec;
      System.Soft_Links'Elab_Spec;
      System.Exception_Table'Elab_Body;
      E023 := E023 + 1;
      Ada.Containers'Elab_Spec;
      E038 := E038 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E068 := E068 + 1;
      Ada.Strings'Elab_Spec;
      E053 := E053 + 1;
      Ada.Strings.Maps'Elab_Spec;
      E055 := E055 + 1;
      Ada.Strings.Maps.Constants'Elab_Spec;
      E059 := E059 + 1;
      Interfaces.C'Elab_Spec;
      E043 := E043 + 1;
      System.Exceptions'Elab_Spec;
      E025 := E025 + 1;
      System.Object_Reader'Elab_Spec;
      E079 := E079 + 1;
      System.Dwarf_Lines'Elab_Spec;
      E048 := E048 + 1;
      System.Os_Lib'Elab_Body;
      E073 := E073 + 1;
      System.Soft_Links.Initialize'Elab_Body;
      E019 := E019 + 1;
      E011 := E011 + 1;
      System.Traceback.Symbolic'Elab_Body;
      E037 := E037 + 1;
      E005 := E005 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Tags'Elab_Body;
      E097 := E097 + 1;
      Ada.Streams'Elab_Spec;
      E149 := E149 + 1;
      Gnat'Elab_Spec;
      E205 := E205 + 1;
      Interfaces.C.Strings'Elab_Spec;
      E218 := E218 + 1;
      System.Finalization_Root'Elab_Spec;
      E151 := E151 + 1;
      Ada.Finalization'Elab_Spec;
      E147 := E147 + 1;
      System.Storage_Pools'Elab_Spec;
      E177 := E177 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Finalization_Masters'Elab_Body;
      E173 := E173 + 1;
      System.Storage_Pools.Subpools'Elab_Spec;
      E171 := E171 + 1;
      Ada.Strings.Unbounded'Elab_Spec;
      E163 := E163 + 1;
      System.Task_Info'Elab_Spec;
      E124 := E124 + 1;
      System.Task_Primitives.Operations'Elab_Body;
      E116 := E116 + 1;
      Ada.Calendar'Elab_Spec;
      Ada.Calendar'Elab_Body;
      E195 := E195 + 1;
      Ada.Calendar.Delays'Elab_Body;
      E193 := E193 + 1;
      Ada.Real_Time'Elab_Spec;
      Ada.Real_Time'Elab_Body;
      E242 := E242 + 1;
      System.Interrupt_Management.Operations'Elab_Body;
      E246 := E246 + 1;
      System.Pool_Global'Elab_Spec;
      E200 := E200 + 1;
      System.Pool_Size'Elab_Spec;
      E222 := E222 + 1;
      Gnat.Sockets'Elab_Spec;
      Gnat.Sockets.Thin_Common'Elab_Spec;
      E216 := E216 + 1;
      E210 := E210 + 1;
      Gnat.Sockets'Elab_Body;
      E207 := E207 + 1;
      System.Tasking.Initialization'Elab_Body;
      E144 := E144 + 1;
      System.Tasking.Protected_Objects'Elab_Body;
      E105 := E105 + 1;
      System.Tasking.Protected_Objects.Entries'Elab_Spec;
      E140 := E140 + 1;
      System.Tasking.Queuing'Elab_Body;
      E157 := E157 + 1;
      System.Tasking.Stages'Elab_Body;
      E248 := E248 + 1;
      System.Tasking.Async_Delays'Elab_Body;
      E240 := E240 + 1;
      Agnostic_Io'Elab_Spec;
      E198 := E198 + 1;
      E103 := E103 + 1;
      Socket_Aio'Elab_Spec;
      Socket_Aio'Elab_Body;
      E204 := E204 + 1;
      Asterisk.Ami'Elab_Spec;
      Asterisk.Ami'Elab_Body;
      E191 := E191 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_test");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer
   is
      procedure Initialize (Addr : System.Address);
      pragma Import (C, Initialize, "__gnat_initialize");

      procedure Finalize;
      pragma Import (C, Finalize, "__gnat_finalize");
      SEH : aliased array (1 .. 2) of Integer;

      Ensure_Reference : aliased System.Address := Ada_Main_Program_Name'Address;
      pragma Volatile (Ensure_Reference);

   begin
      if gnat_argc = 0 then
         gnat_argc := argc;
         gnat_argv := argv;
      end if;
      gnat_envp := envp;

      Initialize (SEH'Address);
      adainit;
      Ada_Main_Program;
      adafinal;
      Finalize;
      return (gnat_exit_status);
   end;

--  BEGIN Object file/option list
   --   /home/oswald/programming/ada/ada-asterisk/examples/obj/debug/test.o
   --   -L/home/oswald/programming/ada/ada-asterisk/examples/obj/debug/
   --   -L/home/oswald/programming/ada/ada-asterisk/examples/obj/debug/
   --   -L/opt/GNAT/2020/lib/aws.static/
   --   -L/opt/GNAT/2020/lib/gnatcoll.static/
   --   -L/opt/GNAT/2020/lib/xmlada/xmlada_dom.static/
   --   -L/opt/GNAT/2020/lib/xmlada/xmlada_sax.static/
   --   -L/opt/GNAT/2020/lib/xmlada/xmlada_unicode.static/
   --   -L/opt/GNAT/2020/lib/xmlada/xmlada_input.static/
   --   -L/opt/GNAT/2020/lib/xmlada/xmlada_schema.static/
   --   -L/opt/GNAT/2020/lib/gpr/static/gpr/
   --   -L/home/oswald/programming/ada/ada-asterisk/lib/debug/static/
   --   -L/home/oswald/programming/ada/ada-asterisk/deps/agnostic_io/lib/debug/static/
   --   -L/opt/GNAT/2020/lib/gcc/x86_64-pc-linux-gnu/9.3.1/adalib/
   --   -static
   --   -lgnarl
   --   -lgnat
   --   -lrt
   --   -lpthread
   --   -ldl
--  END Object file/option list   

end ada_main;
