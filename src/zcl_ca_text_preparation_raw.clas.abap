"! <p class="shorttext synchronized" lang="en">CA-TBX: Preparation of a RAW text</p>
CLASS zcl_ca_text_preparation_raw DEFINITION PUBLIC
                                             INHERITING FROM zcl_ca_text_preparation
                                             FINAL
                                             CREATE PROTECTED
                                             GLOBAL FRIENDS zif_ca_text_preparation.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      "!
      "! @parameter preparation_type | <p class="shorttext synchronized" lang="en">Preparation type (see const ...=&gt;PREPARATION_TYPE-*)</p>
      constructor
        IMPORTING
          preparation_type TYPE zca_d_target_preparation_type
        RAISING
          zcx_ca_text_preparation.


* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   i n s t a n c e   m e t h o d s
    METHODS:
      insert_hint_for_test REDEFINITION,

      set_defaults_in_ctrl_settings REDEFINITION.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Length of mail line / row</p>
      c_length_mail        TYPE i       VALUE 255  ##no_text,
      "! <p class="shorttext synchronized" lang="en">Length of text line / row</p>
      c_length_text        TYPE i       VALUE 132  ##no_text,
      "! <p class="shorttext synchronized" lang="en">Length of requested fields is longer than row length</p>
      c_length_too_long    TYPE i       VALUE 999  ##no_text,
      "! <p class="shorttext synchronized" lang="en">Delimiter for column</p>
      c_coldelim_hash      TYPE char1   VALUE '#'  ##no_text,
      "! <p class="shorttext synchronized" lang="en">Digit 'Tabulator' in hex</p>
      c_coldelim_tabulator TYPE syhex02 VALUE '0900'  ##no_text.

ENDCLASS.                     "zcl_ca_text_module_raw  DEFINITION



CLASS zcl_ca_text_preparation_raw IMPLEMENTATION.

  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*
    super->constructor( preparation_type ).
  ENDMETHOD.                    "constructor


  METHOD insert_hint_for_test.
    "-----------------------------------------------------------------*
    "   Insert hint that text was created/mailed from a test system
    "-----------------------------------------------------------------*
    IF not is_hint_for_test_required( ).
      RETURN.
    ENDIF.

    text_in_preparation->insert_line_at_the_beginning( '<p style="text-align:left"></p>' ) ##no_text.
*    INSERT VALUE #( line = '<p style="text-align:left"></p>' ) INTO text_lines INDEX 1 ##no_text.
*    INSERT VALUE #( line = |<p style="text-align:center">{ get_text_for_hint_of_a_sample( ) } | &
*                           |{ sy-sysid } / { sy-mandt }</p>| ) INTO text_lines INDEX 1 ##no_text.
*    INSERT VALUE #( line = '<p></p>' ) INTO text_lines INDEX 1 ##no_text.


*          ls_text-tdformat = '*'.
*          ls_text-tdline   = space.
*          INSERT ls_text INTO text_lines INDEX 1.
*          ls_text-tdline   = ls_text-tdline = |{ TEXT-mst } { sy-sysid } / { sy-mandt }|.
*          INSERT ls_text INTO text_lines INDEX 1.
*          ls_text-tdline   = space.
*          INSERT ls_text INTO text_lines INDEX 1.
  ENDMETHOD.                    "insert_hint_for_test


  METHOD set_defaults_in_ctrl_settings.
    "-----------------------------------------------------------------*
    "   Provide defaults in control settings if necessary
    "-----------------------------------------------------------------*
    super->set_defaults_in_ctrl_settings( control_settings ).
  ENDMETHOD.                    "set_defaults_in_ctrl_settings

ENDCLASS.                     "zcl_ca_text_module_raw  IMPLEMENTATION


