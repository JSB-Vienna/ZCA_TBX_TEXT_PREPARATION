*----------------------------------------------------------------------*
***INCLUDE LZCA_TEXT_MODULESF01 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  SET_INBOUND_DATA_INTO_GLOBAL
*&---------------------------------------------------------------------*
*       Make data available in global definitions for replacement
*----------------------------------------------------------------------*
*      -->  FIELDS_N_STRUCTURES  Table with data references
*----------------------------------------------------------------------*
FORM set_inbound_data_into_global USING   fields_n_structures TYPE zca_tt_params
                                  RAISING zcx_ca_text_preparation.
  "Local data definitions
  FIELD-SYMBOLS:
    <_inbound_value>     TYPE data,
    <_global_definition> TYPE data.

  "Move inbound values into global structures for symbol replacement
  LOOP AT fields_n_structures INTO  DATA(_field_or_structure)
                              WHERE value IS BOUND.
    "Skip values that are not of a flat structured type
    DATA(_type_description) = cl_abap_typedescr=>describe_by_data_ref( _field_or_structure-value ).
    IF _type_description->type_kind       NE _type_description->typekind_struct1 OR
       _type_description->is_ddic_type( ) NE _type_description->true.
      CONTINUE.
    ENDIF.

    "Get name of inbound value if it is not set
    IF _field_or_structure-name IS INITIAL.
      _field_or_structure-name = _type_description->get_relative_name( ).
    ENDIF.

    TRY.
        "Assign global structure for move
        ASSIGN (_field_or_structure-name) TO <_global_definition>.
        IF sy-subrc NE 0.
          CONTINUE.
        ENDIF.

        "Assign inbound value for move
        ASSIGN _field_or_structure-value->* TO <_inbound_value>.
        <_global_definition> = <_inbound_value>.

      CATCH cx_sy_conversion_error
            cx_sy_assign_error
            cx_sy_move_cast_error  INTO DATA(_catched).
        "Move of inbound value into global structure &1 was not possible
        RAISE EXCEPTION TYPE zcx_ca_text_preparation
          EXPORTING
            textid   = zcx_ca_text_preparation=>move_to_struc_failed
            previous = _catched
            mv_msgv1 = CONV #( _field_or_structure-name ).
    ENDTRY.
  ENDLOOP.
ENDFORM.                    "set_inbound_data_into_global

*&---------------------------------------------------------------------*
*&      Form  REPLACE_STANDARD_SYMBOLS
*&---------------------------------------------------------------------*
*       Replacement of variables/symbols in text module
*----------------------------------------------------------------------*
*      -->  TEXT_MODULE_HEADER  Header of text module
*      <->  TEXT_MODULE_LINES   Text module with replaced standard symbols
*----------------------------------------------------------------------*
FORM replace_standard_symbols USING    text_module_header TYPE thead
                              CHANGING text_module_lines  TYPE tline_tab.
  "Replace standard symbols with data values
  CALL FUNCTION 'TEXT_SYMBOL_REPLACE'
    EXPORTING
      header           = text_module_header
      program          = sy-repid
      replace_program  = abap_true
      replace_standard = abap_true
      replace_system   = abap_true
      replace_text     = abap_true
    TABLES
      lines            = text_module_lines.

  "Resolve includes. If an include could not be resolved, because it isn't
  "available or the current user has no authority, the include line will
  "be printed)
  CALL FUNCTION 'TEXT_INCLUDE_REPLACE'
    EXPORTING
      header  = text_module_header
      program = sy-repid
    TABLES
      lines   = text_module_lines.
ENDFORM.                    "replace_standard_symbols
