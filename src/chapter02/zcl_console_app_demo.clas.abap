CLASS zcl_console_app_demo DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    methods: output.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_console_app_demo IMPLEMENTATION.
  METHOD output.
  cl_demo_output=>display_text( 'This is first line'  ).
  cl_demo_output=>display_text( 'This is second line'  ).
  cl_demo_output=>display_text( 'This is third line'  ).
  ENDMETHOD.

ENDCLASS.
