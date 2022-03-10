FUNCTION z_adtco_get_inc_master_program.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(INCLUDE) TYPE  STRING
*"  EXPORTING
*"     VALUE(MASTER) TYPE  STRING
*"     VALUE(MASTER_TYPE) TYPE  STRING
*"----------------------------------------------------------------------
  SELECT SINGLE master INTO @master
  FROM d010inc
  WHERE include EQ @include.
  IF sy-subrc EQ 0.
    SELECT SINGLE subc FROM trdir
      INTO @DATA(type)
      WHERE name = @master.
    IF type EQ 'F'.
      master_type = 'FUGR/I'.
      IF master(1) EQ '/'.
        DATA(regex) = NEW cl_abap_regex(   pattern       =  '(\/.*\/)SAPL(.*)' ).
        DATA(matcher) = regex->create_matcher( text = master ).
        IF matcher->match( ).
          master = |{ matcher->get_submatch( index = 1 ) }{ matcher->get_submatch( index = 2 ) }|.
        ENDIF.
      ELSE.
        SHIFT master BY 4 PLACES LEFT.
      ENDIF.
    ELSEIF type EQ 'K'.
      DATA offset TYPE i.
      FIND FIRST OCCURRENCE OF '=' IN master MATCH OFFSET offset.
      IF sy-subrc EQ 0.
        master = master(offset).
      ENDIF.
      master_type = 'REPS'.
    ELSE.
      master_type = 'PROG/I'.
    ENDIF.
  ENDIF.



ENDFUNCTION.
