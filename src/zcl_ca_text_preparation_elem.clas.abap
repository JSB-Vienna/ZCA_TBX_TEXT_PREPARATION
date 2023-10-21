"! <p class="shorttext synchronized" lang="en">CA-TBX: Text preparation: Data element / data cell</p>
CLASS zcl_ca_text_preparation_elem DEFINITION PUBLIC
                                              CREATE PROTECTED
                                              GLOBAL FRIENDS zif_ca_text_preparation
                                                             zcl_ca_text_preparation_struct.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Technical description from RTTI</p>
      description   TYPE REF TO cl_abap_elemdescr READ-ONLY,

*     d a t a   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Either the passed value ref. or a created one for a column</p>
      value_ref     TYPE REF TO data READ-ONLY,

*     s t r u c t u r e s
      "! <p class="shorttext synchronized" lang="en">Technical information from DDIC</p>
      ddic_info     TYPE dfies READ-ONLY,

*     s i n g l e   v a l u e s
      "! <p class="shorttext synchronized" lang="en">Alignment of element -&gt; see method DETERMINE_ALIGNMENT</p>
      alignment     TYPE zca_d_table_desc_alignment READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Element name (defined in DDIC or passed by consumer)</p>
      name_to_value TYPE string READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Column position</p>
      position      TYPE int1 READ-ONLY.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      constructor
        IMPORTING
          name_to_value TYPE string
          position      TYPE i OPTIONAL
          value_ref     TYPE REF TO data OPTIONAL
          description   TYPE REF TO cl_abap_typedescr OPTIONAL
          table_row     TYPE REF TO data OPTIONAL
        RAISING
          zcx_ca_text_preparation,

      "! <p class="shorttext synchronized" lang="en">Convert value from internal type into character format</p>
      convert_value_type_conform
        IMPORTING
          currency        TYPE waers OPTIONAL
          unit_of_measure TYPE meins OPTIONAL
        RETURNING
          VALUE(result)   TYPE so_text255
        RAISING
          zcx_ca_conv.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Constants and value checks for text module preparation</p>
      tp_options        TYPE REF TO zcl_ca_c_text_preparation.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      check_value_n_description
        IMPORTING
          value_ref   TYPE REF TO data
          description TYPE REF TO cl_abap_typedescr
        RAISING
          zcx_ca_text_preparation,

      "! <p class="shorttext synchronized" lang="en">Complete technical infos</p>
      complete_technical_infos,

      "! <p class="shorttext synchronized" lang="en">Create data reference for a table column</p>
      create_data_reference
        IMPORTING
          table_row TYPE REF TO data,

      "! <p class="shorttext synchronized" lang="en">Determine alignment depending on type and output length</p>
      determine_alignment.

ENDCLASS.                     "zcl_ca_text_preparation_elem  DEFINITION


CLASS zcl_ca_text_preparation_elem IMPLEMENTATION.

  METHOD check_value_n_description.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*
    TRY.
        IF value_ref IS BOUND.
          me->description ?= tp_options->get_technical_description( value_ref ).
          me->value_ref = value_ref.

        ELSEIF description IS BOUND.
          me->description = CAST cl_abap_elemdescr( description ).

        ELSE.
          "At least one of the following parameters must be passed: &1 &2 &3 &4
          RAISE EXCEPTION TYPE zcx_ca_text_preparation
            EXPORTING
              textid   = zcx_ca_text_preparation=>at_least_one
              mv_msgty = zcx_ca_text_preparation=>c_msgty_e
              mv_msgv1 = 'VALUE_REF'
              mv_msgv2 = 'DESCRIPTION' ##no_text.
        ENDIF.

      CATCH cx_sy_move_cast_error INTO DATA(_catched).
        DATA(_exception) = CAST zcx_ca_text_preparation(
                                     zcx_ca_error=>create_exception(
                                                     iv_excp_cls = zcx_ca_text_preparation=>c_zcx_ca_text_preparation
                                                     ix_error    = _catched ) ) ##no_text.
        IF _exception IS BOUND.
          RAISE EXCEPTION _exception.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "check_value_n_description


  METHOD complete_technical_infos.
    "-----------------------------------------------------------------*
    "   Complete technical infos
    "-----------------------------------------------------------------*
    CASE description->is_ddic_type( ).
      WHEN abap_true.
        "Get DDIC description if typed by DDIC data element
        ddic_info = description->get_ddic_field( ).

      WHEN abap_false.
        "Set values from element description
        ddic_info-inttype   =
        ddic_info-comptype  = description->type_kind.
        ddic_info-intlen    =
        ddic_info-leng      = description->length.
        ddic_info-outputlen = description->output_length.
        ddic_info-decimals  = description->decimals.
        "Set 'with sign' otherwise negative values will not be accepted
        ddic_info-sign      = abap_true.
    ENDCASE.
  ENDMETHOD.                    "complete_technical_infos


  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*
    tp_options = zcl_ca_c_text_preparation=>get_instance( ).

    check_value_n_description( value_ref   = value_ref
                               description = description ).
    me->name_to_value = name_to_value.
    me->position      = position.

    complete_technical_infos( ).
    determine_alignment( ).

    create_data_reference( table_row ).
  ENDMETHOD.                    "constructor


  METHOD create_data_reference.
    "-----------------------------------------------------------------*
    "   Convert value from internal type into character format
    "-----------------------------------------------------------------*
    "Local data definitions
    FIELD-SYMBOLS:
      <table_row> TYPE data,
      <data_cell> TYPE data.

    IF table_row IS NOT BOUND.
      RETURN.
    ENDIF.

    IF name_to_value IS INITIAL.
      "Table row has no structure components, but is defined direct from data element -> results in
      "implicit line component TABLE_LINE.
      value_ref = table_row.

    ELSE.
      "The advantage of this design is, that the individual components/columns are filled
      "simply by the loop in any other class. No further assignment or move is necessary.
      "This is why the column value can be converted in this class.
      ASSIGN table_row->* TO <table_row>.
      ASSERT sy-subrc EQ 0.

      ASSIGN COMPONENT name_to_value OF STRUCTURE <table_row> TO <data_cell>.
      ASSERT sy-subrc EQ 0.
      value_ref = REF #( <data_cell> ).
    ENDIF.
  ENDMETHOD.                    "create_data_reference


  METHOD convert_value_type_conform.
    "-----------------------------------------------------------------*
    "   Convert value from internal type into character format
    "-----------------------------------------------------------------*
    "Local data definitions
    FIELD-SYMBOLS:
      <value>              TYPE data.

    "Assign inbound value for move
    ASSIGN value_ref->* TO <value>.

    "Convert value into output format
    zcl_ca_conv=>internal_2_external(
                                EXPORTING
                                  internal_value  = <value>
                                  currency        = currency
                                  unit_of_measure = unit_of_measure
                                  without_seconds = abap_false
                                IMPORTING
                                  external_value = result ).
  ENDMETHOD.                    "convert_value_type_conform


  METHOD determine_alignment.
    "-----------------------------------------------------------------*
    "   Determine alignment depending on type and output length
    "-----------------------------------------------------------------*
    alignment = COND #( WHEN description->type_kind EQ description->typekind_packed OR
                             description->type_kind EQ description->typekind_num    OR
                             description->type_kind EQ description->typekind_int    OR
                             description->type_kind EQ description->typekind_int1   OR
                             description->type_kind EQ description->typekind_int2   OR
                             description->type_kind EQ description->typekind_int8
                          THEN tp_options->html-alignment-right

                        WHEN description->output_length LE 10
                          THEN tp_options->html-alignment-center

                        ELSE tp_options->html-alignment-left ).
  ENDMETHOD.                    "determine_alignment

ENDCLASS.                     "zcl_ca_text_preparation_elem  IMPLEMENTATION


