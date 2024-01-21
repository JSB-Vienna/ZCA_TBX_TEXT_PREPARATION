"! <p class="shorttext synchronized" lang="en">CA-TBX: Text preparation for a table in HTML</p>
CLASS zcl_ca_text_prep_table_html DEFINITION PUBLIC
                                             INHERITING FROM zcl_ca_text_preparation_table
                                             FINAL
                                             CREATE PROTECTED
                                             GLOBAL FRIENDS zif_ca_text_preparation_table.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      constructor
        IMPORTING
          parent         TYPE REF TO zif_ca_text_preparation
          table_settings TYPE REF TO zca_s_table_in_text
        RAISING
          zcx_ca_param,

      zif_ca_text_preparation_table~prepare_table_rows REDEFINITION,

      zif_ca_text_preparation_table~set_defaults_in_table_settings REDEFINITION.


* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">CSS elements</p>
      BEGIN OF css_element,
        BEGIN OF border,
          base     TYPE its_tag VALUE '{ border:'  ##no_text,
          collapse TYPE its_tag VALUE 'border-collapse: collapse }'  ##no_text,
        END   OF border,
      END   OF css_element,

      "! <p class="shorttext synchronized" lang="en">HTML tags</p>
      BEGIN OF html_tag,
        BEGIN OF table,
          open  TYPE its_tag VALUE '<table>'  ##no_text,
          close TYPE its_tag VALUE '</table>'  ##no_text,
        END   OF table,

        BEGIN OF table_header,
          open  TYPE its_tag VALUE '<thead>'  ##no_text,
          close TYPE its_tag VALUE '</thead>'  ##no_text,
        END   OF table_header,

        BEGIN OF header_column,
          open  TYPE its_tag VALUE '<th>'  ##no_text,
          close TYPE its_tag VALUE '</th>'  ##no_text,
        END   OF header_column,

        BEGIN OF table_body,
          open  TYPE its_tag VALUE '<tbody>'  ##no_text,
          close TYPE its_tag VALUE '</tbody>'  ##no_text,
        END   OF table_body,

        BEGIN OF table_row,
          open  TYPE its_tag VALUE '<tr>'  ##no_text,
          close TYPE its_tag VALUE '</tr>'  ##no_text,
        END   OF table_row,

        BEGIN OF data_column,
          open  TYPE its_tag VALUE '<td>'  ##no_text,
          close TYPE its_tag VALUE '</td>'  ##no_text,
        END   OF data_column,

        line_break TYPE its_tag VALUE '<br>'  ##no_text,
      END   OF html_tag.

*   i n s t a n c e   m e t h o d s
    METHODS:
      add_table_header REDEFINITION,

      add_table_body REDEFINITION,

      add_table_caption REDEFINITION,

      create_n_add_column_header REDEFINITION,

      create_n_add_table_body REDEFINITION,

      create_n_attach_col_head_cell REDEFINITION,

      create_n_attach_data_cell REDEFINITION,

      "! <p class="shorttext synchronized" lang="en">Enhance style for the cell tags 'td'+'th' to align the value</p>
      get_style_for_cell_alignment
        IMPORTING
          alignment     TYPE zca_d_table_desc_alignment
        RETURNING
          VALUE(result) TYPE string,

      "! <p class="shorttext synchronized" lang="en">Create style addition for the tags 'table', 'th' and 'td'</p>
      get_style_for_border_n_padding
        RETURNING
          VALUE(result) TYPE string,

      prepare_cell_value_for_output REDEFINITION,

      "! <p class="shorttext synchronized" lang="en">Translate alignment code into HTML key word</p>
      translate_alignment_2_html
        IMPORTING
          alignment     TYPE zca_d_table_desc_alignment
        RETURNING
          VALUE(result) TYPE string,

      "! <p class="shorttext synchronized" lang="en">Translate frame style code into HTML key word</p>
      translate_frame_style_2_html
        IMPORTING
          frame_style   TYPE zca_d_frame_style
        RETURNING
          VALUE(result) TYPE string,

      "! <p class="shorttext synchronized" lang="en">Translate frame width code into HTML key word</p>
      translate_frame_width_2_html
        IMPORTING
          frame_width   TYPE zca_d_frame_width
        RETURNING
          VALUE(result) TYPE string.



* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Add font for table</p>
      get_font_style_for_table
        RETURNING
          VALUE(result) TYPE string,

      "! <p class="shorttext synchronized" lang="en">Set alternative font family if req. font isn't installed</p>
      set_alternative_font_family
        RETURNING
          VALUE(result) TYPE string,

      "! <p class="shorttext synchronized" lang="en">Set font size in 'em'</p>
      set_font_size
        RETURNING
          VALUE(result) TYPE string,

      "! <p class="shorttext synchronized" lang="en">Set requested font for this table</p>
      set_requested_font
        RETURNING
          VALUE(result) TYPE string.

ENDCLASS.                     "zcl_ca_text_prep_table_html  DEFINITION



CLASS zcl_ca_text_prep_table_html IMPLEMENTATION.

  METHOD add_table_body.
    "-----------------------------------------------------------------*
    "   Add data rows of table (body) to table of prepared rows
    "-----------------------------------------------------------------*
*    table_in_preparation->add_line_at_the_end( CONV #( html_tag-table_body-open ) ).      "<tbody>
    table_in_preparation->add_line_at_the_end( condense( |<tbody style="{ get_font_style_for_table( ) }">| ) ) ##no_text.

    create_n_add_table_body( ).

    table_in_preparation->add_line_at_the_end( CONV #( html_tag-table_body-close ) ).      "</tbody>
  ENDMETHOD.                    "add_table_body


  METHOD add_table_caption.
    "-----------------------------------------------------------------*
    "   Add caption (heading) of table
    "-----------------------------------------------------------------*
    IF NOT is_table_caption_available( ).
      RETURN.
    ENDIF.

    "Caption must be first statement after open <TABLE> tag
    DATA(_caption_alignment) = translate_alignment_2_html( settings->table_desc_alignm ).
    "The next commented lines are an example how to get the caption with further individual values, e. g. for
    "weight and font-style into the prepared lines.
*    line_in_preparation = |<caption style="|.
*    attach_prepared_value_2_line( |text-weight:{ translate_wight_code_2_html( settings->font_weight ) }; | ).
*    attach_prepared_value_2_line( |font-style:{ translate_font_style_2_html( settings->font_weight ) }; | ).
*    attach_prepared_value_2_line( |text-align: { _caption_alignment }">{ settings->table_desc }</caption>| ).
*    table_in_preparation->add_line_at_the_end( line_in_preparation ).
    table_in_preparation->add_line_at_the_end(
                            |<caption style="font-weight: bold; font-style: italic; | &
*                            |<caption style="font-style: italic; | &
                            |text-align: { _caption_alignment }">{ settings->table_desc }</caption>| ) ##no_text.
  ENDMETHOD.                    "add_table_caption


  METHOD add_table_header.
    "-----------------------------------------------------------------*
    "   Add table / column header to table of prepared rows
    "-----------------------------------------------------------------*
    IF NOT is_column_header_requested( ).
      RETURN.
    ENDIF.

*    table_in_preparation->add_line_at_the_end( CONV #( html_tag-table_header-open ) ).      "<thead>
    table_in_preparation->add_line_at_the_end( condense( |<thead style="{ get_font_style_for_table( ) }">| ) ) ##no_text.

    create_n_add_column_header( ).

    table_in_preparation->add_line_at_the_end( CONV #( html_tag-table_header-close ) ).     "</thead>
  ENDMETHOD.                    "add_table_header


  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*
    super->constructor( parent         = parent
                        table_settings = table_settings ).
  ENDMETHOD.                    "constructor


  METHOD create_n_add_column_header.
    "-----------------------------------------------------------------*
    "   Create column header
    "-----------------------------------------------------------------*
    "Initialize line in preparation with row tag
    table_in_preparation->add_line_at_the_end( CONV #( html_tag-table_row-open ) ).     "<tr>
*    table_in_preparation->add_line_at_the_end( condense( |<tr style="{ get_style_for_border_n_padding( ) }">| ) ) ##no_text.

    CASE settings->sign_for_flds.
      WHEN sel_options->sign-incl.
        LOOP AT settings->t_output_flds REFERENCE INTO output_field.
          row_component = VALUE #( techn_row->components[ KEY primary_key
                                                          name = output_field->fieldname ]-element OPTIONAL ).
          IF row_component IS BOUND.
            create_n_attach_col_head_cell( ).
          ENDIF.
        ENDLOOP.

      WHEN sel_options->sign-excl.
        LOOP AT techn_row->components USING KEY primary_key
                                      REFERENCE INTO DATA(_component)
                                          WHERE name IN requested_columns.
          output_field  = REF #( settings->t_output_flds[ fieldname = _component->name ] OPTIONAL ).
          IF output_field IS BOUND.
            row_component = _component->element.
            create_n_attach_col_head_cell( ).
          ENDIF.
        ENDLOOP.
    ENDCASE.

    table_in_preparation->add_line_at_the_end( CONV #( html_tag-table_row-close ) ).   "</tr>
  ENDMETHOD.                    "create_n_add_column_header


  METHOD create_n_add_table_body.
    "-----------------------------------------------------------------*
    "   Create table body and add each row to table of prepared rows
    "-----------------------------------------------------------------*
    "Local data definitions
    FIELD-SYMBOLS:
      <table_data> TYPE ANY TABLE,
      <table_row>  TYPE data.

    "Make table data accessible
    ASSIGN settings->value->* TO <table_data>.
    ASSERT sy-subrc EQ 0.

    "Type conform row / workarea was already created during construction of this instance
    ASSIGN techn_row->table_row->* TO <table_row>.
    ASSERT sy-subrc EQ 0.

    LOOP AT <table_data> INTO <table_row>.
      "Initialize prepared row with row tag
      table_in_preparation->add_line_at_the_end( CONV #( html_tag-table_row-open ) ).     "<tr>
*      table_in_preparation->add_line_at_the_end( condense( |<tr style="{ get_style_for_border_n_padding( ) }">| ) ) ##no_text.

      CASE settings->sign_for_flds.
        WHEN sel_options->sign-incl.
          LOOP AT settings->t_output_flds REFERENCE INTO output_field.
            row_component = VALUE #( techn_row->components[ KEY primary_key
                                                            name = output_field->fieldname ]-element OPTIONAL ).
            IF row_component IS BOUND.
              create_n_attach_data_cell( ).
            ENDIF.
          ENDLOOP.

        WHEN sel_options->sign-excl.
          LOOP AT techn_row->components USING KEY primary_key
                                        REFERENCE INTO DATA(_component)
                                            WHERE name IN requested_columns.
            output_field  = REF #( settings->t_output_flds[ fieldname = _component->name ] OPTIONAL ).
            IF output_field IS BOUND.
              row_component = _component->element.
              create_n_attach_data_cell( ).
            ENDIF.
          ENDLOOP.
      ENDCASE.

      table_in_preparation->add_line_at_the_end( CONV #( html_tag-table_row-close ) ).   "</tr>
    ENDLOOP.
  ENDMETHOD.                    "create_n_add_table_body


  METHOD create_n_attach_col_head_cell.
    "-----------------------------------------------------------------*
    "   Create and attach single column header to prepared row
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      _column_header_label TYPE text2048.

    DATA(_field_label) = determine_field_label( ).

    IF _field_label IS INITIAL.
      _column_header_label = condense( |{ html_tag-header_column-open }{ html_tag-header_column-close }| ).

    ELSE.
      "Alignment of component is determined during instance creation of technical structure infos. Numeric
      "values are aligned always right, output length lower equal 10 are center aligned, the rest to the left.
      "Adding the cell alignment is always necessary because no CSS can be used and without this addition no
      "frame is visible.
      _column_header_label = condense( |<th style="{ get_style_for_border_n_padding( ) }; | &
                                       |{ get_style_for_cell_alignment( row_component->alignment ) }">| &
                                       |{ _field_label }{ html_tag-header_column-close }| ).
    ENDIF.

    table_in_preparation->add_line_at_the_end( _column_header_label ).
  ENDMETHOD.                    "create_n_attach_col_head_cell


  METHOD create_n_attach_data_cell.
    "-----------------------------------------------------------------*
    "   Create and attach single data column to prepared row
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      _prepared_value      TYPE text2048.

    DATA(_cell_value) = prepare_cell_value_for_output( ).

    IF _cell_value IS INITIAL.
      _prepared_value = condense( |{ html_tag-data_column-open }{ html_tag-data_column-close }| ).

    ELSE.
      "Alignment of component is determined during instance creation of technical structure infos. Numeric
      "values are aligned always right, output length lower equal 10 are center aligned, the rest to the left.
      "Adding the cell alignment is always necessary because no CSS can be used and without this addition no
      "frame is visible.
      _prepared_value = condense( |<td style="{ get_style_for_border_n_padding( ) }; | &
                                  |{ get_style_for_cell_alignment( row_component->alignment ) }">| &
                                  |{ _cell_value }{ html_tag-data_column-close }| ).
    ENDIF.

    table_in_preparation->add_line_at_the_end( _prepared_value ).
  ENDMETHOD.                    "create_n_attach_data_cell


  METHOD get_font_style_for_table.
    "-----------------------------------------------------------------*
    "   Set tag and replace font, alternative font and font size by passed values
    "-----------------------------------------------------------------*
    "Adding the apostrophe around a font name is a recommendation of W3C for fonts with a blank in the name
    "Result example:
    "font-family:Courier New, monospace; font-size:1em
    result = |font-family:{ set_requested_font( ) }, { set_alternative_font_family( ) }; | &
             |font-size:{ set_font_size( ) }| ##no_text.
  ENDMETHOD.                    "get_font_style_for_table


  METHOD get_style_for_border_n_padding.
    "-----------------------------------------------------------------*
    "   Create style addition for border and padding of table cells
    "-----------------------------------------------------------------*
    "Result example:
    "border-collapse: collapse; border:medium solid #F3C431; padding:0.5rem 0.25rem
    result = |border-collapse:collapse; border:{ translate_frame_width_2_html( settings->frame_width ) } | &
             |{ translate_frame_style_2_html( CONV #( settings->with_frame ) ) } { settings->frame_color }; | &
             "                 upper and lower          left and right
             |padding:{ settings->v_padding }rem { settings->h_padding }rem| ##no_text.
  ENDMETHOD.                    "get_style_for_border_n_padding


  METHOD get_style_for_cell_alignment.
    "-----------------------------------------------------------------*
    "   Enhance style for the cell tags 'td' + 'th' to align the value
    "-----------------------------------------------------------------*
    "Result example:
    "text-align:right
    result = |text-align:{ translate_alignment_2_html( alignment ) }| ##no_text.
  ENDMETHOD.                    "get_style_for_cell_alignment


  METHOD prepare_cell_value_for_output.
    "-----------------------------------------------------------------*
    "   Prepare cell value for output
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      _no_currency        TYPE waers VALUE space,
      _no_unit_of_measure TYPE meins VALUE space.

    FIELD-SYMBOLS:
      <table_row>       TYPE data,
      <currency>        TYPE waers,
      <unit_of_measure> TYPE meins.

    TRY.
        "Type conform row / workarea was already created during construction of this instance
        ASSIGN techn_row->table_row->* TO <table_row>.
        ASSERT sy-subrc EQ 0.

        IF output_field                        IS BOUND AND
           output_field->fld_name_currency_key IS NOT INITIAL.
          ASSIGN COMPONENT output_field->fld_name_currency_key OF STRUCTURE <table_row> TO <currency>.
          ASSERT sy-subrc EQ 0.
          ASSIGN _no_unit_of_measure TO <unit_of_measure>.

        ELSEIF output_field                    IS BOUND AND
               output_field->fld_name_unit_key IS NOT INITIAL.
          ASSIGN COMPONENT output_field->fld_name_unit_key OF STRUCTURE <table_row> TO <unit_of_measure>.
          ASSERT sy-subrc EQ 0.
          ASSIGN _no_currency TO <currency>.

        ELSE.
          ASSIGN: _no_currency        TO <currency>,
                  _no_unit_of_measure TO <unit_of_measure>.
          ASSERT sy-subrc EQ 0.
        ENDIF.

        "The of the row component is already available in the ROW_COMPONENT and must not be passed
        result = row_component->convert_value_type_conform( currency        = <currency>
                                                            unit_of_measure = <unit_of_measure> ).

      CATCH zcx_ca_error INTO DATA(lx_catched).
        DATA(lx_error) = CAST zcx_ca_text_preparation(
                                     zcx_ca_error=>create_exception(
                                                     iv_excp_cls = zcx_ca_text_preparation=>c_zcx_ca_text_preparation
                                                     ix_error    = lx_catched ) ).
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "prepare_cell_value_for_output


  METHOD set_alternative_font_family.
    "-----------------------------------------------------------------*
    "   Set alternative font family in case the requested font isn't installed
    "-----------------------------------------------------------------*
    result = SWITCH #( control_settings->fonttype_prop
               WHEN tp_options->html-defaults-font_type-proportional
                 THEN `sans-serif`
               WHEN tp_options->html-defaults-font_type-monospace
                 THEN `monospace` ) ##no_text.

  ENDMETHOD.                    "set_alternative_font_family


  METHOD set_font_size.
    "-----------------------------------------------------------------*
    "   Set font size in 'rem'
    "-----------------------------------------------------------------*
    result = SWITCH #( settings->use_prop_font
               WHEN boolean->true
                 THEN COND #( WHEN control_settings->fontsize_prop EQ '0'
                                THEN `inherit` ELSE |{ control_settings->fontsize_prop }rem| )
               WHEN boolean->false
                 THEN COND #( WHEN control_settings->fontsize_monospc EQ '0'
                                THEN `inherit` ELSE |{ control_settings->fontsize_monospc }rem| ) ) ##no_text.
  ENDMETHOD.                    "set_font_size


  METHOD set_requested_font.
    "-----------------------------------------------------------------*
    "   Set requested font for this table
    "-----------------------------------------------------------------*
    result = SWITCH #( settings->use_prop_font
               WHEN boolean->true
                 THEN control_settings->font_prop
               WHEN boolean->false
                 THEN control_settings->font_monospc ).
  ENDMETHOD.                    "set_requested_font


  METHOD translate_alignment_2_html.
    "-----------------------------------------------------------------*
    "   Translate alignment code into HTML key word
    "-----------------------------------------------------------------*
    result = SWITCH #( alignment
               WHEN tp_options->html-alignment-center THEN `center`
               WHEN tp_options->html-alignment-left   THEN `left`
               WHEN tp_options->html-alignment-right  THEN `right`
                 ELSE `inherit` ) ##no_text.
  ENDMETHOD.                    "translate_alignment_2_html


  METHOD translate_frame_style_2_html.
    "-----------------------------------------------------------------*
    "   Translate alignment code into HTML key word
    "-----------------------------------------------------------------*
    result = SWITCH #( frame_style
               WHEN tp_options->html-frame_style-none  THEN `none`
               WHEN tp_options->html-frame_style-solid THEN `solid`
               WHEN tp_options->html-frame_style-ridge THEN `ridge`
                 ELSE `inherit` ) ##no_text.
  ENDMETHOD.                    "translate_alignm_code_2_html


  METHOD translate_frame_width_2_html.
    "-----------------------------------------------------------------*
    "   Translate alignment code into HTML key word
    "-----------------------------------------------------------------*
    result = SWITCH #( frame_width
               WHEN tp_options->html-frame_width-thin   THEN `thin`
               WHEN tp_options->html-frame_width-medium THEN `medium`
               WHEN tp_options->html-frame_width-thick  THEN `thick`
                 ELSE `inherit` ) ##no_text.
  ENDMETHOD.                    "translate_alignm_code_2_html


  METHOD zif_ca_text_preparation_table~prepare_table_rows.
    "-----------------------------------------------------------------*
    "   Prepare table rows for output
    "-----------------------------------------------------------------*
    " ! ! !  Don't change the order of these method calls  ! ! !  This is important for composition
*    table_in_preparation->add_line_at_the_end( CONV #( html_tag-table-open ) ).     "<table>
    table_in_preparation->add_line_at_the_end(
*           condense( |<table style="{ get_style_for_border_n_padding( ) }">| ) ) ##no_text.
           condense( |<table style="{ get_style_for_border_n_padding( ) }; { get_font_style_for_table( ) }">| ) ) ##no_text.

    add_table_caption( ).
    add_table_header( ).
    add_table_body( ).

    table_in_preparation->add_line_at_the_end( CONV #( html_tag-table-close ) ).     "</table>
    result = table_in_preparation->text_lines.
  ENDMETHOD.                    "zif_ca_text_preparation_table~prepare_table_rows


  METHOD zif_ca_text_preparation_table~set_defaults_in_table_settings.
    "-----------------------------------------------------------------*
    "   Set default values for non-provided table settings
    "-----------------------------------------------------------------*
    super->set_defaults_in_table_settings( ).

    IF settings->tab_delimited CN '01'.      "only 0 (= boolean->false) and 1 (= boolean->true) allowed
      settings->tab_delimited = boolean->true.
    ENDIF.

    IF settings->use_prop_font CN '01'.
      settings->use_prop_font = boolean->true.
    ENDIF.

    IF settings->with_frame CN '01'.
      settings->with_frame = boolean->false.
    ENDIF.

    CASE settings->with_frame.
      WHEN boolean->true.
        IF settings->frame_width IS INITIAL.
          settings->frame_width = tp_options->html-frame_width-thin.
        ELSE.
          tp_options->is_frame_width_valid( settings->frame_width ).
        ENDIF.

        IF settings->frame_style IS INITIAL.
          settings->frame_style = tp_options->html-frame_style-solid.
        ELSE.
          tp_options->is_frame_style_valid( settings->frame_style ).
        ENDIF.

        IF settings->frame_color IS INITIAL.
          IF control_settings->without_head_body EQ boolean->true.     "Is e. g. for enhancement in Fiori Inbox
            settings->frame_color = tp_options->html-frame_width-inherit.
          ELSE.
            settings->frame_color = 'black' ##no_text.      "Is e. g. for a mail
          ENDIF.
        ENDIF.

      WHEN boolean->false.
        IF settings->frame_width IS INITIAL.
          settings->frame_width = tp_options->html-frame_width-inherit.
        ENDIF.
        IF settings->frame_style IS INITIAL.
          settings->frame_style = tp_options->html-frame_style-inherit.
        ENDIF.
        IF settings->frame_color IS INITIAL.
          settings->frame_color = tp_options->html-frame_width-inherit.
        ENDIF.
    ENDCASE.

    IF settings->v_padding IS INITIAL.
      settings->v_padding = '0.25'.     "Upper and lower space between value and border
    ENDIF.

    IF settings->h_padding IS INITIAL.
      settings->h_padding = '0.5'.      "Left and right space between value and border
    ENDIF.

    IF settings->table_desc_alignm IS INITIAL.
      settings->table_desc_alignm = tp_options->html-alignment-center.
    ELSE.
      tp_options->is_alignment_valid( settings->table_desc_alignm ).
    ENDIF.
  ENDMETHOD.                    "zif_ca_text_preparation_table~set_defaults_in_table_settings

ENDCLASS.                     "zcl_ca_text_prep_table_html  IMPLEMENTATION


