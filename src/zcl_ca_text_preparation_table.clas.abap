"! <p class="shorttext synchronized" lang="en">CA-TBX: Text preparation of table values</p>
CLASS zcl_ca_text_preparation_table DEFINITION PUBLIC
                                               ABSTRACT
                                               CREATE PROTECTED.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n t e r f a c e s
    INTERFACES:
      zif_ca_text_preparation_table.

*   a l i a s e s
    ALIASES:
*     Table preparation - Attributes
      control_settings               FOR  zif_ca_text_preparation_table~control_settings,
      settings                       FOR  zif_ca_text_preparation_table~settings,
      techn_table                    FOR  zif_ca_text_preparation_table~techn_table,

*     Table preparation - Methods
      prepare_table_rows             FOR  zif_ca_text_preparation_table~prepare_table_rows,
      set_defaults_in_table_settings FOR  zif_ca_text_preparation_table~set_defaults_in_table_settings.

**   i n s t a n c e   a t t r i b u t e s
*    DATA:
**     o b j e c t   r e f e r e n c e s
*      "! <p class="shorttext synchronized" lang="en">Technical description of the table</p>
*      techn_descr TYPE REF TO cl_abap_tabledescr READ-ONLY.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Get instance to corresponding preparation type</p>
      "!
      "! @parameter parent                  | <p class="shorttext synchronized" lang="en">CA-TBX: Preparation of a text module</p>
      "! @parameter table_settings          | <p class="shorttext synchronized" lang="en">Table settings</p>
      "! @raising   zcx_ca_text_preparation | <p class="shorttext synchronized" lang="en">CA-TBX exception: While preparing text module</p>
      get_instance
        IMPORTING
          parent         TYPE REF TO zif_ca_text_preparation
          table_settings TYPE REF TO zca_s_table_in_text
        RETURNING
          VALUE(result)  TYPE REF TO zif_ca_text_preparation_table
        RAISING
          zcx_ca_text_preparation.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      "!
      "! @raising   zcx_ca_text_preparation | <p class="shorttext synchronized" lang="en">CA-TBX exception: While preparing text module</p>
      constructor
        IMPORTING
          parent         TYPE REF TO zif_ca_text_preparation
          table_settings TYPE REF TO zca_s_table_in_text
        RAISING
          zcx_ca_text_preparation.


* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">CA-TBX: Constants for numeric boolean flags</p>
      boolean              TYPE REF TO zcl_ca_c_numeric_boolean,
      "! <p class="shorttext synchronized" lang="en">CA-TBX: Preparation of a text module</p>
      parent               TYPE REF TO zif_ca_text_preparation,
      "! <p class="shorttext synchronized" lang="en">CA-TBX: Constants and value checks for select option tables</p>
      sel_options          TYPE REF TO zcl_ca_c_sel_options,
      "! <p class="shorttext synchronized" lang="en">CA-TBX: Text preparation: Techn. details of used structure</p>
      techn_row            TYPE REF TO zcl_ca_text_preparation_struct,
      "! <p class="shorttext synchronized" lang="en">CA-TBX: Text preparation: Data element / data cell</p>
      row_component        TYPE REF TO zcl_ca_text_preparation_elem,
      "! <p class="shorttext synchronized" lang="en">CA-TBX: Text preparation: Current table in preparation</p>
      table_in_preparation TYPE REF TO zcl_ca_text_prepared_lines,
*      "! <p class="shorttext synchronized" lang="en">CA-TBX: Text preparation: Complete text in preparation for output</p>
*      text_in_preparation TYPE REF TO zcl_ca_text_prepared_lines,
      "! <p class="shorttext synchronized" lang="en">Constants and value checks for text module preparation</p>
      tp_options           TYPE REF TO zcl_ca_c_text_preparation,

*     d a t a   r e f e r e n c e s
*      "! <p class="shorttext synchronized" lang="en">Dynamically created reference for data row</p>
*      table_row            TYPE REF TO data,
      "! <p class="shorttext synchronized" lang="en">Details to current output field</p>
      output_field         TYPE REF TO zca_s_output_field,

*     t a b l e s
*      "! <p class="shorttext synchronized" lang="en">Prepared table rows</p>
*      prep_table_rows     TYPE zca_tt_text_lines,
      "! <p class="shorttext synchronized" lang="en">Range table with excluding or including requested columns</p>
      requested_columns    TYPE rsdsselopt_t,

*     s t r u c t u r e s
      "! <p class="shorttext synchronized" lang="en">Prepared table row currently in preparation</p>
      line_in_preparation  TYPE text2048.
*      "! <p class="shorttext synchronized" lang="en">Description</p>
*      ms_...               TYPE x..
*
**     s i n g l e   v a l u e s
*      "! <p class="shorttext synchronized" lang="en">Description</p>
*      mv_...               TYPE x..

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Add data rows of table (body) to table of prepared rows</p>
      add_table_body ABSTRACT
        RAISING
          zcx_ca_text_preparation,

      "! <p class="shorttext synchronized" lang="en">Add caption (heading) of table</p>
      add_table_caption ABSTRACT,

      "! <p class="shorttext synchronized" lang="en">Add table / column header to table of prepared rows</p>
      "!
      "! @raising   zcx_ca_text_preparation | <p class="shorttext synchronized" lang="en">CA-TBX exception: While preparing text module</p>
      add_table_header ABSTRACT
        RAISING
          zcx_ca_text_preparation,

      "! <p class="shorttext synchronized" lang="en">Attach prepared value to current row</p>
      attach_prepared_value_2_line
        IMPORTING
          prepared_value TYPE text200,

      "! <p class="shorttext synchronized" lang="en">Create column header and add to table of prepared rows</p>
      "!
      "! @raising   zcx_ca_text_preparation | <p class="shorttext synchronized" lang="en">CA-TBX exception: While preparing text module</p>
      create_n_add_column_header ABSTRACT
        RAISING
          zcx_ca_text_preparation,

      "! <p class="shorttext synchronized" lang="en">Create and attach single column header to prepared row</p>
      "!
      "! @raising   zcx_ca_text_preparation | <p class="shorttext synchronized" lang="en">CA-TBX exception: While preparing text module</p>
      create_n_attach_col_head_cell ABSTRACT
        RAISING
          zcx_ca_text_preparation,

      "! <p class="shorttext synchronized" lang="en">Create and attach single data column to prepared row</p>
      "!
      "! @raising   zcx_ca_text_preparation | <p class="shorttext synchronized" lang="en">CA-TBX exception: While preparing text module</p>
      create_n_attach_data_cell ABSTRACT
        RAISING
          zcx_ca_text_preparation,

      "! <p class="shorttext synchronized" lang="en">Create table body and add each row to table of prepared rows</p>
      "!
      "! @raising   zcx_ca_text_preparation | <p class="shorttext synchronized" lang="en">CA-TBX exception: While preparing text module</p>
      create_n_add_table_body ABSTRACT
        RAISING
          zcx_ca_text_preparation,

      "! <p class="shorttext synchronized" lang="en">Determine field label as column header text</p>
      determine_field_label
        RETURNING
          VALUE(result) TYPE text200,

      "! <p class="shorttext synchronized" lang="en">Determine requested columns</p>
      determine_requested_columns,

      "! <p class="shorttext synchronized" lang="en">Get technical row description preparing add. techn. infos</p>
      "!
      "! @raising   zcx_ca_text_preparation | <p class="shorttext synchronized" lang="en">CA-TBX exception: While preparing text module</p>
      get_row_description
        RAISING
          zcx_ca_text_preparation,

      "! <p class="shorttext synchronized" lang="en">Check whether the value is of type table and has content</p>
      "!
      "! @parameter result                  | <p class="shorttext synchronized" lang="en">X = Value is a table and has content</p>
      "! @raising   zcx_ca_text_preparation | <p class="shorttext synchronized" lang="en">CA-TBX exception: While preparing text module</p>
      is_a_table_with_content
        RETURNING
          VALUE(result) TYPE abap_boolean
        RAISING
          zcx_ca_text_preparation,

      "! <p class="shorttext synchronized" lang="en">Check whether the column header is requested</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">X = Column header is requested</p>
      is_column_header_requested
        RETURNING
          VALUE(result) TYPE abap_boolean,

      "! <p class="shorttext synchronized" lang="en">Is table caption available?</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">X = Table caption is available</p>
      is_table_caption_available
        RETURNING
          VALUE(result) TYPE abap_boolean,

      "! <p class="shorttext synchronized" lang="en">Check whether the name is set -&gt; no replacment possible</p>
      "!
      "! @raising   zcx_ca_text_preparation | <p class="shorttext synchronized" lang="en">CA-TBX exception: While preparing text module</p>
      is_name_set
        RAISING
          zcx_ca_text_preparation,

      "! <p class="shorttext synchronized" lang="en">Check whether the value is of internal type table</p>
      "!
      "! @raising   zcx_ca_text_preparation | <p class="shorttext synchronized" lang="en">CA-TBX exception: While preparing text module</p>
      is_value_a_table
        RAISING
          zcx_ca_text_preparation,

      "! <p class="shorttext synchronized" lang="en">Check whether the value is bound</p>
      "!
      "! @raising   zcx_ca_text_preparation | <p class="shorttext synchronized" lang="en">CA-TBX exception: While preparing text module</p>
      is_value_bound
        RAISING
          zcx_ca_text_preparation,

      "! <p class="shorttext synchronized" lang="en">Prepare cell value for output</p>
      "!
      "! @parameter result                  | <p class="shorttext synchronized" lang="en">Prepared value for output</p>
      "! @raising   zcx_ca_text_preparation | <p class="shorttext synchronized" lang="en">CA-TBX exception: While preparing text module</p>
      prepare_cell_value_for_output ABSTRACT
        RETURNING
          VALUE(result) TYPE text200
        RAISING
          zcx_ca_text_preparation,

      "! <p class="shorttext synchronized" lang="en">Prepare order of requested columns</p>
      prepare_order_of_req_columns.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
**   i n s t a n c e   m e t h o d s
*    METHODS:
*      "! <p class="shorttext synchronized" lang="en">Set default values for non-provided table settings</p>
*      set_defaults_in_table_settings.

ENDCLASS.                     "zcl_ca_text_preparation_table  DEFINITION



CLASS zcl_ca_text_preparation_table IMPLEMENTATION.

  METHOD attach_prepared_value_2_line.
    "-----------------------------------------------------------------*
    "   Attach prepared value to current row
    "-----------------------------------------------------------------*
    "No CONDENSE here!!! This has to be decided by the subclass if it is needed or not!!
    line_in_preparation = |{ line_in_preparation }{ prepared_value }|.
  ENDMETHOD.                    "attach_prepared_value_2_line


  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*
    boolean     = zcl_ca_c_numeric_boolean=>get_instance( ).
    sel_options = zcl_ca_c_sel_options=>get_instance( ).
    tp_options  = zcl_ca_c_text_preparation=>get_instance( ).

    me->parent           = parent.
    "Local text lines instance for the preparation of this table. There no text lines or text key has to be passed.
    table_in_preparation = NEW #( parent = parent ).
    control_settings     = REF #( parent->control_settings ).
    settings             = table_settings.

    "Checks
    is_name_set( ).
    is_a_table_with_content( ).

    "Technical preparations
    get_row_description( ).
  ENDMETHOD.                    "constructor


  METHOD determine_field_label.
    "-----------------------------------------------------------------*
    "   Determine field label as column header text
    "-----------------------------------------------------------------*
    "If no labels are provided use those of the DDIC
    IF output_field->s_field_labels IS INITIAL.
      output_field->s_field_labels = CORRESPONDING #( row_component->ddic_info ).
    ENDIF.

    result = COND #( WHEN row_component->ddic_info-outputlen LE 10
                       THEN output_field->scrtext_s
                     WHEN row_component->ddic_info-outputlen LE 20
                       THEN output_field->scrtext_m
                       ELSE output_field->scrtext_l ).

    IF result IS INITIAL.
      result = COND #( WHEN row_component->ddic_info-outputlen LE 10
                         THEN output_field->scrtext_m
                         ELSE output_field->scrtext_l ).
    ENDIF.

    IF result IS INITIAL.
      result = output_field->scrtext_l.
    ENDIF.
  ENDMETHOD.                    "determine_field_label


  METHOD determine_requested_columns.
    "-----------------------------------------------------------------*
    "   Determine requested columns
    "-----------------------------------------------------------------*
    "Create a range for the requested fields
    IF settings->t_output_flds IS NOT INITIAL.
      requested_columns = VALUE #( FOR _output_field IN settings->t_output_flds
                                                           ( sign   = settings->sign_for_flds
                                                             option = sel_options->option-eq
                                                             low    = _output_field-fieldname ) ).

    ELSE.
      "Put all fields of the structures into the settings. Only elementary components are left in TECHN_ROW!
      settings->t_output_flds = VALUE #( FOR _component_for_output IN techn_row->components
                                                               ( col_order = _component_for_output-position
                                                                 fieldname = _component_for_output-name ) ).
    ENDIF.
  ENDMETHOD.                    "determine_requested_columns


  METHOD get_instance.
    "-----------------------------------------------------------------*
    "   Get instance to corresponding preparation type
    "-----------------------------------------------------------------*
    DATA(_tp_options) = zcl_ca_c_text_preparation=>get_instance( ).
    _tp_options->is_preparation_type_valid( parent->preparation_type ).

    TRY.
        DATA(_class_name) = |ZCL_CA_TEXT_PREP_TABLE_{ parent->preparation_type }| ##no_text.
        CREATE OBJECT result TYPE (_class_name)
          EXPORTING
            parent         = parent
            table_settings = table_settings.

        "Do this here after the constructor due to subclasses redefining this method
        result->set_defaults_in_table_settings( ).

      CATCH cx_sy_create_object_error INTO DATA(_catched).
        MESSAGE _catched TYPE zcx_ca_error=>c_msgty_e.
    ENDTRY.
  ENDMETHOD.                    "Get_instance


  METHOD get_row_description.
    "-----------------------------------------------------------------*
    "   Get technical row description preparing add. techn. infos
    "-----------------------------------------------------------------*
    techn_row = zcl_ca_text_preparation_struct=>get_instance( techn_table->get_table_line_type( ) ).
  ENDMETHOD.                    "get_row_description


  METHOD is_a_table_with_content.
    "-----------------------------------------------------------------*
    "   Check whether the value is of type table and has content
    "-----------------------------------------------------------------*
    TRY.
        result = abap_false.

        is_value_bound( ).
        techn_table ?= tp_options->get_technical_description( settings->value ).
        is_value_a_table( ).

        "Dereference table object and check if it has data
        ASSIGN settings->value->* TO FIELD-SYMBOL(<table_data>).
        ASSERT sy-subrc EQ 0.

        IF <table_data> IS INITIAL.
          RETURN.
        ENDIF.

        result = abap_true.

      CATCH cx_sy_move_cast_error INTO DATA(_catched).
        DATA(_exception) = CAST zcx_ca_text_preparation(
                                      zcx_ca_error=>create_exception(
                                                   iv_excp_cls = zcx_ca_text_preparation=>c_zcx_ca_text_preparation
                                                   ix_error    = _catched ) ) ##no_text.
        IF _exception IS BOUND.
          RAISE EXCEPTION _exception.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "is_a_table_with_content


  METHOD is_column_header_requested.
    "-----------------------------------------------------------------*
    "   Check whether the column header is requested
    "-----------------------------------------------------------------*
    result = xsdbool( settings->without_hdr EQ boolean->false ).
  ENDMETHOD.                    "is_column_header_requested


  METHOD is_name_set.
    "-----------------------------------------------------------------*
    "   Check whether the name is set (otherwise no replacement possible)
    "-----------------------------------------------------------------*
    IF settings->name IS INITIAL.
      "Parameter '&1' has invalid value '&2'
      RAISE EXCEPTION TYPE zcx_ca_text_preparation
        EXPORTING
          textid   = zcx_ca_text_preparation=>param_invalid
          mv_msgty = zcx_ca_text_preparation=>c_msgty_e
          mv_msgv1 = 'NAME is not set' ##no_text.
    ENDIF.
  ENDMETHOD.                    "is_name_set


  METHOD is_table_caption_available.
    "-----------------------------------------------------------------*
    "   Is table caption available?
    "-----------------------------------------------------------------*
    result = xsdbool( settings->table_desc IS NOT INITIAL ).
  ENDMETHOD.                    "is_table_caption_available


  METHOD is_value_a_table.
    "-----------------------------------------------------------------*
    "   Check whether the value is of internal type table
    "-----------------------------------------------------------------*
    IF techn_table->kind NE techn_table->kind_table.
      "Value to table parameter &1 is not a table
      RAISE EXCEPTION TYPE zcx_ca_text_preparation
        EXPORTING
          textid   = zcx_ca_text_preparation=>not_a_table
          mv_msgv1 = CONV #( settings->name ).
    ENDIF.
  ENDMETHOD.                    "is_value_a_table


  METHOD is_value_bound.
    "-----------------------------------------------------------------*
    "   Check whether the value is bound
    "-----------------------------------------------------------------*
    IF settings->value IS NOT BOUND.
      "Parameter '&1' has invalid value '&2'
      RAISE EXCEPTION TYPE zcx_ca_text_preparation
        EXPORTING
          textid   = zcx_ca_text_preparation=>param_invalid
          mv_msgty = zcx_ca_text_preparation=>c_msgty_e
          mv_msgv1 = 'VALUE is not bound' ##no_text.
    ENDIF.
  ENDMETHOD.                    "is_value_bound


  METHOD prepare_order_of_req_columns.
    "-----------------------------------------------------------------*
    "   Prepare order of requested columns
    "-----------------------------------------------------------------*
    "Is an output order requested?
    LOOP AT settings->t_output_flds TRANSPORTING NO FIELDS
                                    WHERE col_order IS NOT INITIAL.
      EXIT.
    ENDLOOP.
    "Found a column order? Then sort it by the requested order. Otherwise keep it as it is!
    IF sy-subrc EQ 0.
      SORT settings->t_output_flds BY col_order fieldname.

    ELSE.
      "Set column order
      DATA(_column_order) = 0.
      LOOP AT settings->t_output_flds REFERENCE INTO DATA(_output_field).
        _column_order += 1.
        _output_field->col_order = _column_order.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.                    "prepare_order_of_req_columns


  METHOD zif_ca_text_preparation_table~prepare_table_rows.
    "-----------------------------------------------------------------*
    "   Prepare table rows for output
    "-----------------------------------------------------------------*
    "H a s   t o   b e   r e d e f i n i e d
  ENDMETHOD.                    "zif_ca_text_preparation_table~prepare_table_rows


  METHOD zif_ca_text_preparation_table~set_defaults_in_table_settings.
    "-----------------------------------------------------------------*
    "   Set default values for non-provided table settings
    "-----------------------------------------------------------------*
    "Keep the execution order of this IF statement and the following methods as they depend on each other.
    IF settings->sign_for_flds CN 'IE' ##no_text.
      settings->sign_for_flds = sel_options->sign-incl.    "Include columns is default
    ENDIF.

    IF settings->sign_for_flds EQ sel_options->sign-excl AND
       settings->t_output_flds IS INITIAL.
      "The combination of &1 and &2 will result in no output - adapt parameters
      RAISE EXCEPTION TYPE zcx_ca_text_preparation
        EXPORTING
          textid   = zcx_ca_text_preparation=>no_result_due_to_param_combi
          mv_msgty = zcx_ca_text_preparation=>c_msgty_e
          mv_msgv1 = |SIGN_FOR_FLDS = 'E'|
          mv_msgv2 = |T_OUTPUT_FLDS = INITIAL| ##no_text.
    ENDIF.

    determine_requested_columns( ).
    prepare_order_of_req_columns( ).

    IF settings->without_hdr CN '01'.
      settings->without_hdr = boolean->false.    "With column header is default
    ENDIF.

    IF settings->col_optimized CN '01'.
      settings->col_optimized = boolean->true.    "Optimize columns is default
    ENDIF.
  ENDMETHOD.                    "zif_ca_text_preparation_table~set_defaults_in_table_settings

ENDCLASS.                     "zcl_ca_text_preparation_table  IMPLEMENTATION


