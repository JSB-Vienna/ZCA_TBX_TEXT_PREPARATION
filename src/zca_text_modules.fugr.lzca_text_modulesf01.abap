*----------------------------------------------------------------------*
***INCLUDE LZCA_TEXT_MODULESF01 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  SET_INBOUND_DATA_INTO_GLOBAL
*&---------------------------------------------------------------------*
*       Make data available in global definitions for replacement
*----------------------------------------------------------------------*
*      -->  IT_STRUCTS  Table with data references
*----------------------------------------------------------------------*
FORM set_inbound_data_into_global USING   it_strucs TYPE zca_tt_params
                                  RAISING zcx_ca_text_module.
  "Local data definitions
  DATA:
    ls_struc     TYPE zca_s_param,
    lo_type_desc TYPE REF TO cl_abap_typedescr,
    lx_error     TYPE REF TO cx_root.

  FIELD-SYMBOLS:
    <ls_value>  TYPE data,
    <ls_global> TYPE data.

  "Move inbound values into global structures for symbol replacement
  LOOP AT it_strucs INTO  ls_struc
                    WHERE value IS BOUND.
    "Get name of inbound value if it is not set
    lo_type_desc = cl_abap_typedescr=>describe_by_data_ref(
                                                       ls_struc-value ).
    "Skip values that are not of flat structured type
    IF lo_type_desc->type_kind       NE lo_type_desc->typekind_struct1 OR
       lo_type_desc->is_ddic_type( ) NE lo_type_desc->true.
      CONTINUE.
    ENDIF.

    "Get DDIC name of object
    ls_struc-name = lo_type_desc->get_relative_name( ).

    TRY.
        "Assign global structure for move
        ASSIGN (ls_struc-name) TO <ls_global>.
        IF sy-subrc NE 0.
          CONTINUE.
        ENDIF.

        "Assign inbound value for move
        ASSIGN ls_struc-value->* TO <ls_value>.

        "Move inbound value to global field
        <ls_global> = <ls_value>.

      CATCH cx_sy_assign_cast_illegal_cast
            cx_sy_assign_cast_unknown_type
            cx_sy_assign_out_of_range
            cx_sy_conversion_no_number
            cx_sy_conversion_overflow
            cx_sy_move_cast_error      INTO lx_error.
        sy-msgv1 = ls_struc-name.
        "Move of inbound value into global structure &1 was not possible
        RAISE EXCEPTION TYPE zcx_ca_text_module
          EXPORTING
            textid   = zcx_ca_text_module=>move_to_struc_failed
            previous = lx_error
            mv_msgv1 = sy-msgv1.
    ENDTRY.
  ENDLOOP.
ENDFORM.                    "set_inbound_data_into_global

*&---------------------------------------------------------------------*
*&      Form  REPLACE_STANDARD_SYMBOLS
*&---------------------------------------------------------------------*
*       Replacement of variables/symbols in text module
*----------------------------------------------------------------------*
*      -->  IS_THEAD  Header of text module
*      <->  CT_TEXT   Text module with replaced standard symbols
*----------------------------------------------------------------------*
FORM replace_standard_symbols USING    is_thead TYPE thead
                              CHANGING ct_text  TYPE tline_tab.
  "Replace standard symbols with data values
  CALL FUNCTION 'TEXT_SYMBOL_REPLACE'
    EXPORTING
      header           = is_thead
      program          = sy-repid
      replace_program  = abap_true
      replace_standard = abap_true
      replace_system   = abap_true
      replace_text     = abap_true
    TABLES
      lines            = ct_text.

  "Resolve includes. If an include could not be resolved, because it isn't
  "available or the current user has no authority, the include line will
  "be printed)
  CALL FUNCTION 'TEXT_INCLUDE_REPLACE'
    EXPORTING
      header  = is_thead
      program = sy-repid
    TABLES
      lines   = ct_text.
ENDFORM.                    "replace_standard_symbols
