"! <p class="shorttext synchronized" lang="en">CA-TBX: Preparation of a HTML text</p>
CLASS zcl_ca_text_preparation_html DEFINITION PUBLIC
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
      "! @parameter preparation_type        | <p class="shorttext synchronized" lang="en">Preparation type (see const ...=&gt;PREPARATION_TYPE-*)</p>
      "! @raising   zcx_ca_text_preparation | <p class="shorttext synchronized" lang="en">CA-TBX exception: While preparing text module</p>
      constructor
        IMPORTING
          preparation_type TYPE zca_d_target_preparation_type
        RAISING
          zcx_ca_text_preparation,

      "! <p class="shorttext synchronized" lang="en">Is HTML DOM area requested?</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">X = Yes, create HTML DOM area (head and body tags)</p>
      is_html_dom_requested
        RETURNING
          VALUE(result) TYPE abap_boolean,

      zif_ca_text_preparation~replace_n_transform REDEFINITION.


* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Surround document in tags for HTML declaration and body</p>
      add_overall_surrounding_tags,

      insert_hint_for_test REDEFINITION,

      replace_link_symbols REDEFINITION,

      set_defaults_in_ctrl_settings REDEFINITION.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     d a t a   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">CA-TBX: Description for link and the link itself</p>
      link_settings             TYPE REF TO zca_s_text_n_link.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Assemble link</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">Assembled link</p>
      assemble_link
        RETURNING
          VALUE(result) TYPE text2048,

      "! <p class="shorttext synchronized" lang="en">Get description to link</p>
      "!
      "! @parameter result                  | <p class="shorttext synchronized" lang="en">CA-TBX: Text preparation: Text in preparation for output</p>
      "! @raising   zcx_ca_text_preparation | <p class="shorttext synchronized" lang="en">CA-TBX exception: While preparing text module</p>
      get_link_description
        RETURNING
          VALUE(result) TYPE REF TO zcl_ca_text_prepared_lines
        RAISING
          zcx_ca_text_preparation,

      "! <p class="shorttext synchronized" lang="en">Assemble link and set description in front of it</p>
      "!
      "! @parameter result                  | <p class="shorttext synchronized" lang="en">Prepared lines with description and link</p>
      "! @raising   zcx_ca_text_preparation | <p class="shorttext synchronized" lang="en">CA-TBX exception: While preparing text module</p>
      prepare_link_n_its_description
        RETURNING
          VALUE(result) TYPE zca_tt_text_lines
        RAISING
          zcx_ca_text_preparation,

      "! <p class="shorttext synchronized" lang="en">Set defaults for link preparation settings</p>
      set_defaults_in_link_settings.

ENDCLASS.                     "zcl_ca_text_preparation_html  DEFINITION


CLASS zcl_ca_text_preparation_html IMPLEMENTATION.

  METHOD add_overall_surrounding_tags.
    "-----------------------------------------------------------------*
    "   Surround completed document in tags for HTML declaration and body
    "-----------------------------------------------------------------*
    "This routine sets completion of the body and the complete header definition from botton to top, except
    "the CSS definition for the tables and the closing element of the HTML document.

    "Leave, if no DOM objects are requested
    IF NOT is_html_dom_requested( ).
      RETURN.
    ENDIF.

    "Set tag and replace font and font size by defined values.
    text_in_preparation->insert_line_at_the_beginning( CONV #( tp_options->html_tag-body-open ) ) ##no_text.   "<body>
    text_in_preparation->insert_line_at_the_beginning( CONV #( tp_options->html_tag-head-close ) ) ##no_text.  "</head>
    text_in_preparation->insert_line_at_the_beginning( |<meta charset="{ control_settings-charset }">| ) ##no_text.
    text_in_preparation->insert_line_at_the_beginning( CONV #( tp_options->html_tag-head-open ) ) ##no_text.   "<head>
    text_in_preparation->insert_line_at_the_beginning( CONV #( tp_options->html_tag-html-open ) ) ##no_text.   "<html>
    text_in_preparation->insert_line_at_the_beginning( |<!doctype html>| ) ##no_text.

    text_in_preparation->add_line_at_the_end( CONV #( tp_options->html_tag-body-close ) ) ##no_text.   "<body>
    text_in_preparation->add_line_at_the_end( CONV #( tp_options->html_tag-html-close ) ) ##no_text.   "<html>
  ENDMETHOD.                    "add_overall_surrounding_tags


  METHOD assemble_link.
    "-----------------------------------------------------------------*
    "   Assemble link
    "-----------------------------------------------------------------*
    IF link_settings->link IS INITIAL.
      result = link_settings->name.    "To let the consumer know which link is incomplete
      RETURN.
    ENDIF.

    " <a href="https://www.xxxx.yyy/folder/index.html" target="_blank">Manual for xxxx</a>
    result = |<a href="{ link_settings->link }" target="_blank">| &  "_blank = open in new window or new tab
             |{ COND text2048( WHEN link_settings->link_name IS INITIAL
                                 THEN 'Link'(lnk) ELSE link_settings->link_name ) }</a>| ##no_text.
  ENDMETHOD.                    "assemble_link


  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*
    super->constructor( preparation_type ).
  ENDMETHOD.                    "constructor


  METHOD get_link_description.
    "-----------------------------------------------------------------*
    "   Get description to link
    "-----------------------------------------------------------------*
    IF link_settings->s_text_key IS NOT INITIAL.
      result = NEW #( parent          = me
                      text_module_key = link_settings->s_text_key ).
      result->replace_sap_script_symbols( ).
*      result->add_line_at_the_end( CONV #( tp_options->html_tag-line_break ) ).  "To force the link into the next line

    ELSEIF link_settings->link_desc IS NOT INITIAL.
      result = NEW #( parent     = me                                            "To force the link into the next line
                      text_lines = VALUE #( ( line = |{ link_settings->link_desc }{ tp_options->html_tag-line_break }| ) ) ).

    ELSE.
      result = NEW #( parent = me ).     "No description available
    ENDIF.
  ENDMETHOD.                    "get_link_description


  METHOD insert_hint_for_test.
    "-----------------------------------------------------------------*
    "   Insert hint that text was created/mailed from a test system
    "-----------------------------------------------------------------*
    IF NOT is_hint_for_test_required( ).
      RETURN.
    ENDIF.

    text_in_preparation->insert_line_at_the_beginning( '<p></p>' ) ##no_text.
    text_in_preparation->insert_line_at_the_beginning(
                |<h1 style="letter-spacing:0.2em; text-align:center; color:red">{ get_text_for_hint_of_a_sample( ) } | &
                |{ sy-sysid } / { sy-mandt }</h1>| ) ##no_text.
    text_in_preparation->insert_line_at_the_beginning( '<p></p>' ) ##no_text.
  ENDMETHOD.                    "insert_hint_for_test


  METHOD is_html_dom_requested.
    "-----------------------------------------------------------------*
    "   Is HTML DOM area requested?
    "-----------------------------------------------------------------*
    result = xsdbool( control_settings-without_head_body EQ boolean->false ).
  ENDMETHOD.                    "is_html_dom_requested


  METHOD prepare_link_n_its_description.
    "-----------------------------------------------------------------*
    "   Assemble link and set description in front of it
    "-----------------------------------------------------------------*
    DATA(_link_descr_in_preparation) = get_link_description( ).

    _link_descr_in_preparation->add_line_at_the_end(
                              |{ assemble_link( ) }{ SWITCH #( link_settings->force_line_break
                                                       WHEN boolean->true  THEN tp_options->html_tag-line_break
                                                       WHEN boolean->false THEN space ) }| ).

    IF link_settings->force_init_line EQ boolean->true.
      _link_descr_in_preparation->add_line_at_the_end( CONV #( tp_options->html_tag-line_break ) ).
    ENDIF.

    result = _link_descr_in_preparation->text_lines.
  ENDMETHOD.                    "prepare_link_n_its_description


  METHOD replace_link_symbols.
    "-----------------------------------------------------------------*
    "   Replace link symbols
    "-----------------------------------------------------------------*
    "Prepare each link and replace the corresponding symbol
    LOOP AT links REFERENCE INTO link_settings.
      TRY.
          IF NOT text_in_preparation->does_symbol_occur_in_text( link_settings->name ).
            CONTINUE.
          ENDIF.

          set_defaults_in_link_settings( ).
          text_in_preparation->exchange_symbol_by_table_rows( prepare_link_n_its_description( ) ).

        CATCH zcx_ca_error INTO DATA(_catched).
          DATA(_exception) = CAST zcx_ca_text_preparation(
                                      zcx_ca_error=>create_exception(
                                                     iv_excp_cls = zcx_ca_text_preparation=>c_zcx_ca_text_preparation
                                                     ix_error    = _catched ) ) ##no_text.
          IF _exception IS BOUND.
            RAISE EXCEPTION _exception.
          ENDIF.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.                    "replace_link_symbols


  METHOD set_defaults_in_ctrl_settings.
    "-----------------------------------------------------------------*
    "   Provide defaults in control settings if necessary
    "-----------------------------------------------------------------*
    super->set_defaults_in_ctrl_settings( control_settings ).

    "Set defaults if no relevant values available
    "P r o p o r t i o n a l
    IF me->control_settings-font_prop IS INITIAL.
      me->control_settings-font_prop = tp_options->html-defaults-font-proportional.
    ENDIF.

    IF me->control_settings-fontsize_prop IS NOT INITIAL.
      tp_options->is_font_size_valid( me->control_settings-fontsize_prop ).
    ELSE.
      me->control_settings-fontsize_prop = tp_options->html-defaults-font_size-proportional.
    ENDIF.

    IF me->control_settings-fonttype_prop IS INITIAL.
      me->control_settings-fonttype_prop = tp_options->html-defaults-font_type-proportional.
    ENDIF.

    "M o n o s p a c e d
    IF me->control_settings-font_monospc IS INITIAL.
      me->control_settings-font_monospc = tp_options->html-defaults-font-monospace.
    ENDIF.

    IF me->control_settings-fontsize_monospc IS NOT INITIAL.
      tp_options->is_font_size_valid( me->control_settings-fontsize_monospc ).
    ELSE.
      me->control_settings-fontsize_monospc = tp_options->html-defaults-font_size-monospace.
    ENDIF.

    IF me->control_settings-fonttype_monospc IS INITIAL.
      me->control_settings-fonttype_monospc = tp_options->html-defaults-font_type-monospace.
    ENDIF.

    "Set character set
    IF control_settings-charset IS INITIAL.
      TRY.
          me->control_settings-charset = cl_abap_codepage=>current( http_name = abap_true ).

        CATCH cx_parameter_invalid.
          me->control_settings-charset = 'utf-8' ##no_text.
      ENDTRY.
    ENDIF.

    "Set with DOM (tags for head and body)
    IF me->control_settings-without_head_body CN '01'.
      me->control_settings-without_head_body = boolean->false.
    ENDIF.
  ENDMETHOD.                    "set_defaults_in_ctrl_settings


  METHOD set_defaults_in_link_settings.
    "-----------------------------------------------------------------*
    "   Set defaults for link preparation settings
    "-----------------------------------------------------------------*
    "Set defaults if no relevant values available

    "Force a line break at the end of the link
    IF link_settings->force_line_break CN '01'.
      link_settings->force_line_break = boolean->true.
    ENDIF.

    "Force a initial line below the link
    IF link_settings->force_init_line CN '01'.
      link_settings->force_init_line = boolean->false.
    ENDIF.
  ENDMETHOD.                    "set_defaults_in_link_settings


  METHOD zif_ca_text_preparation~replace_n_transform.
    "-----------------------------------------------------------------*
    "   Replace variables and transform text module into target format
    "-----------------------------------------------------------------*
    super->replace_n_transform( control_settings    = control_settings
                                fields_n_structures = fields_n_structures
                                tables              = tables
                                links               = links ).

    "Surround the document with the tags for head and body
    add_overall_surrounding_tags( ).
  ENDMETHOD.                    "zif_ca_text_preparation~replace_n_transform

ENDCLASS.                     "zcl_ca_text_preparation_html  IMPLEMENTATION


