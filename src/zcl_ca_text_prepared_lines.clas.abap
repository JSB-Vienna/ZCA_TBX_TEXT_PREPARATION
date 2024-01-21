"! <p class="shorttext synchronized" lang="en">CA-TBX: Text preparation: Text in preparation for output</p>
CLASS zcl_ca_text_prepared_lines DEFINITION PUBLIC
                                            FINAL
                                            CREATE PROTECTED
                                            GLOBAL FRIENDS zif_ca_text_preparation
                                                           zif_ca_text_preparation_table.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Append a line to text lines at the end</p>
      add_line_at_the_end
        IMPORTING
          line TYPE text2048,

      "! <p class="shorttext synchronized" lang="en">Are text lines already passed?</p>
      "!
      "! @raising   zcx_ca_text_preparation | <p class="shorttext synchronized" lang="en">CA-TBX exception: While preparing text module</p>
      are_text_lines_passed
        RAISING
          zcx_ca_text_preparation,

      "! <p class="shorttext synchronized" lang="en">Checks whether the text contains the MAIL_HTML command</p>
      contains_mail_html_command
        RETURNING
          VALUE(result) TYPE abap_boolean,

      "! <p class="shorttext synchronized" lang="en">Returns whether a symbol occur in the text lines</p>
      does_symbol_occur_in_text
        IMPORTING
          symbol_name   TYPE rs38l_par_
        RETURNING
          VALUE(result) TYPE abap_boolean,

      "! <p class="shorttext synchronized" lang="en">Exchange table symbol by prepared table rows</p>
      exchange_symbol_by_table_rows
        IMPORTING
          prep_table_rows TYPE zca_tt_text_lines,

      "! <p class="shorttext synchronized" lang="en">Get text in current state, e. g. for additional manipulation</p>
      get_text
        RETURNING
          VALUE(text_lines) TYPE zca_tt_text_lines,

      "! <p class="shorttext synchronized" lang="en">Insert a line into text lines at the beginning (= index 1)</p>
      insert_line_at_the_beginning
        IMPORTING
          line TYPE text2048,

      "! <p class="shorttext synchronized" lang="en">Remove unused symbols</p>
      "!
      "! @raising   zcx_ca_text_preparation | <p class="shorttext synchronized" lang="en">CA-TBX exception: While preparing text module</p>
      remove_unused_symbols
        RAISING
          zcx_ca_text_preparation,

      "! <p class="shorttext synchronized" lang="en">Replace SAP script symbols (= structured values)</p>
      "!
      "! @raising   zcx_ca_text_preparation | <p class="shorttext synchronized" lang="en">CA-TBX exception: While preparing text module</p>
      replace_sap_script_symbols
        RAISING
          zcx_ca_text_preparation,

      "! <p class="shorttext synchronized" lang="en">Replace symbol by value in prepared text lines</p>
      "!
      replace_symbol_by_value
        IMPORTING
          name  TYPE rs38l_par_
          value TYPE so_text255,

      "! <p class="shorttext synchronized" lang="en">Set text, e. g. after additional manipulation</p>
      set_text
        importing
          text_lines TYPE zca_tt_text_lines.


* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      "!
      "! @parameter parent                  | <p class="shorttext synchronized" lang="en">CA-TBX: Text module preparation</p>
      "! @parameter text_module_key         | <p class="shorttext synchronized" lang="en">Text module key (has to be maintained in SO10)</p>
      "! @parameter text_lines              | <p class="shorttext synchronized" lang="en">Table that contains the text lines to be prepared</p>
      "! @raising   zcx_ca_text_preparation | <p class="shorttext synchronized" lang="en">CA-TBX exception: While preparing text module</p>
      constructor
        IMPORTING
          parent          TYPE REF TO zif_ca_text_preparation
          text_module_key TYPE stxh_key OPTIONAL
          text_lines      TYPE zca_tt_text_lines OPTIONAL
        RAISING
          zcx_ca_text_preparation,

      "! <p class="shorttext synchronized" lang="en">Create symbols for search (name is bracketed in hashes)</p>
      create_symbol_from_value_name
        IMPORTING
          name_to_value TYPE rs38l_par_
        RETURNING
          VALUE(result) TYPE string,

      "! <p class="shorttext synchronized" lang="en">Returns whether the text is a text module</p>
      is_a_text_module
        RETURNING
          VALUE(result) TYPE abap_boolean.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">CA-TBX: Text module preparation</p>
      parent             TYPE REF TO zif_ca_text_preparation,
      "! <p class="shorttext synchronized" lang="en">CA-TBX: Constants + value checks for text module preparation</p>
      tp_options         TYPE REF TO zcl_ca_c_text_preparation,

*     t a b l e s
      "! <p class="shorttext synchronized" lang="en">Text lines (either imported from table or text module)</p>
      text_lines         TYPE zca_tt_text_lines,
      "! <p class="shorttext synchronized" lang="en">Text module lines (until replacement via SAP script FMs)</p>
      text_module_lines  TYPE tline_tab,

*     s t r u c t u r e s
      "! <p class="shorttext synchronized" lang="en">Result of FIND / REPLACE statements</p>
      matching_result    TYPE match_result,
      "! <p class="shorttext synchronized" lang="en">Text module header (includes the text module key!)</p>
      text_module_header TYPE thead.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Convert text lines into text module (SAP-Script)</p>
      get_in_text_module_format
        RETURNING
          VALUE(result) TYPE tline_tab,

      "! <p class="shorttext synchronized" lang="en">Read lines of text module</p>
      "!
      "! @parameter text_module_key         | <p class="shorttext synchronized" lang="en">Text module key (has to be maintained in SO10)</p>
      "! @raising   zcx_ca_text_preparation | <p class="shorttext synchronized" lang="en">CA-TBX exception: While preparing text module</p>
      read_text_module
        IMPORTING
          text_module_key TYPE stxh_key
        RAISING
          zcx_ca_text_preparation,

      "! <p class="shorttext synchronized" lang="en">(Re-)Convert text module (SAP-Script) into text lines</p>
      reconvert_into_txt_lines.

ENDCLASS.                     "zcl_ca_text_prepared_lines  DEFINITION



CLASS zcl_ca_text_prepared_lines IMPLEMENTATION.

  METHOD add_line_at_the_end.
    "-----------------------------------------------------------------*
    "   Append a line to text lines at the end
    "-----------------------------------------------------------------*
    APPEND VALUE #( line = line ) TO  text_lines.
  ENDMETHOD.                    "add_line_at_the_end


  METHOD are_text_lines_passed.
    "-----------------------------------------------------------------*
    "   Are text lines already passed?
    "-----------------------------------------------------------------*
    IF text_module_lines IS INITIAL AND
       text_lines        IS INITIAL.
      "Provide either a text module or text lines first before using preparation
      RAISE EXCEPTION TYPE zcx_ca_text_preparation
        EXPORTING
          textid   = zcx_ca_text_preparation=>no_text_available
          mv_msgty = zcx_ca_text_preparation=>c_msgty_e.
    ENDIF.
  ENDMETHOD.                    "are_text_lines_passed


  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*
    me->parent = parent.
    tp_options = zcl_ca_c_text_preparation=>get_instance( ).

    "No exception here if nothing was passed -> in some cases it's ok to start empty (= no base text)
    IF text_lines IS NOT INITIAL.
      CASE parent->preparation_type.
        WHEN parent->tp_options->preparation_type-html.
          "Add a line break to each raw line before preparation starts. To do it later, is much worse.
          me->text_lines = VALUE #( FOR _text_line IN text_lines
                                          ( line = CONV #( |{ _text_line-line }{ tp_options->html_tag-line_break }| ) ) ).

        WHEN parent->tp_options->preparation_type-raw.
          me->text_lines = text_lines.
      ENDCASE.

    ELSEIF text_module_key IS NOT INITIAL.
      read_text_module( text_module_key ).
      "Provide the text lines immediately to allow the consumer to start right away without using the SAP script
      "replacements. In the case the consumer uses the SAP script replacements it will overwrite the
      "current text lines. This means also that the SAP script replacements MUST always be the first!!!
      reconvert_into_txt_lines( ).
    ENDIF.
  ENDMETHOD.                    "constructor


  METHOD contains_mail_html_command.
    "-----------------------------------------------------------------*
    "   Check whether the text contains the MAIL_HTML command
    "-----------------------------------------------------------------*
    result = abap_false.
    DATA(_first_line) = REF #( text_lines[ 1 ] OPTIONAL ).
    IF _first_line IS BOUND AND
       _first_line->line CS tp_options->techn_addition-mail_html_command.
      result = abap_true.
    ENDIF.
  ENDMETHOD.                    "contains_mail_html_command


  METHOD create_symbol_from_value_name.
    "-----------------------------------------------------------------*
    "   Create symbols for search (name bracketed in hashes)
    "-----------------------------------------------------------------*
    " 'T_NOTES    ....   ' becomes #T_NOTES#
    result = condense( |#{ to_upper( name_to_value ) }#| ).
  ENDMETHOD.                    "create_symbol_from_value_name


  METHOD does_symbol_occur_in_text.
    "-----------------------------------------------------------------*
    "   Returns whether a table symbol exist in the text lines or not
    "-----------------------------------------------------------------*
    result = abap_false.
    IF symbol_name IS INITIAL.
      RETURN.
    ENDIF.

    DATA(_symbol) = create_symbol_from_value_name( symbol_name ).

    CLEAR matching_result.
    "Search for a symbol like #symbol# in the text lines and keep the result for further use
    FIND FIRST OCCURRENCE OF SUBSTRING _symbol
                              IN TABLE text_lines IN CHARACTER MODE
                               RESULTS matching_result.
    IF sy-subrc EQ 0.
      result = abap_true.
    ENDIF.
  ENDMETHOD.                    "does_symbol_occur_in_text


  METHOD exchange_symbol_by_table_rows.
    "-----------------------------------------------------------------*
    "   Exchange table symbol by prepared table rows
    "-----------------------------------------------------------------*
    "Delete line with table symbol in any case to avoid a symbol if
    "no data were passed.
    DELETE text_lines INDEX matching_result-line.

    IF prep_table_rows IS NOT INITIAL.
      "Insert prepared tables lines into text
      INSERT LINES OF prep_table_rows INTO  text_lines
                                      INDEX matching_result-line.
    ENDIF.
  ENDMETHOD.                    "exchange_symbol_by_table_rows


  METHOD get_in_text_module_format.
    "-----------------------------------------------------------------*
    "   Convert passed text lines into SAP script format
    "-----------------------------------------------------------------*
    "No conversion necessary if the source text was a text module anyway
    IF is_a_text_module( ).
      RETURN.
    ENDIF.

    IF contains_mail_html_command( ).
      text_module_lines = VALUE #( FOR _text_line IN text_lines
                                          ( tdformat = '*'
                                            tdline   = _text_line-line ) ).

    ELSE.
      CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
        EXPORTING
          language    = 'E'             "in English in any case
          lf          = abap_false      "because parameter TEXT_STREAM is used (not STREAM_LINES)
          iv_fw       = 132
          iv_lw       = 132
        TABLES
          text_stream = text_lines
          itf_text    = text_module_lines.
    ENDIF.
  ENDMETHOD.                    "get_in_text_module_format


  METHOD get_text.
    "-----------------------------------------------------------------*
    "   Get text in current state, e. g. for additional manipulation
    "-----------------------------------------------------------------*
    text_lines = me->text_lines.
  ENDMETHOD.                    "get_text


  METHOD insert_line_at_the_beginning.
    "-----------------------------------------------------------------*
    "   Insert a line into text lines at the beginning (= index 1)
    "-----------------------------------------------------------------*
    INSERT VALUE #( line = line ) INTO  text_lines
                                  INDEX 1.
  ENDMETHOD.                    "insert_line_at_the_beginning


  METHOD is_a_text_module.
    "-----------------------------------------------------------------*
    "   Returns whether the text is a text module
    "-----------------------------------------------------------------*
    result = xsdbool( text_module_header IS NOT INITIAL ).
  ENDMETHOD.                    "read_text_module


  METHOD read_text_module.
    "-----------------------------------------------------------------*
    "   Get text module
    "-----------------------------------------------------------------*
    "Get text module and header
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        name                    = text_module_key-tdname
        object                  = COND #( WHEN text_module_key-tdobject IS NOT INITIAL
                                            THEN text_module_key-tdobject ELSE 'TEXT' )
        id                      = COND #( WHEN text_module_key-tdid IS NOT INITIAL
                                            THEN text_module_key-tdid ELSE 'ST' )
        language                = COND #( WHEN text_module_key-tdspras IS NOT INITIAL
                                            THEN text_module_key-tdspras ELSE sy-langu )
      IMPORTING
        header                  = text_module_header
      TABLES
        lines                   = text_module_lines
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF sy-subrc EQ 0.
      "Text module was found

    ELSEIF sy-subrc BETWEEN 1 AND 6.
      DATA(_exception) = CAST zcx_ca_text_preparation(
                                    zcx_ca_error=>create_exception(
                                                   iv_excp_cls = zcx_ca_text_preparation=>c_zcx_ca_text_preparation
                                                   iv_function = 'READ_TEXT'
                                                   iv_subrc    = sy-subrc ) ) ##no_text.
      IF _exception IS BOUND.
        RAISE EXCEPTION _exception.
      ENDIF.

    ELSE.
      DATA(_internal_exception) = zcx_ca_intern=>create_exception(
                                                         iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                                         iv_function = 'READ_TEXT'
                                                         iv_subrc    =  sy-subrc ) ##no_text.
      RAISE EXCEPTION _internal_exception.
    ENDIF.
  ENDMETHOD.                    "read_text_module


  METHOD reconvert_into_txt_lines.
    "-----------------------------------------------------------------*
    "   (Re-)Convert text module (SAP-Script) into text lines
    "-----------------------------------------------------------------*
    CASE parent->preparation_type.
      WHEN parent->tp_options->preparation_type-html.
        "Add a line break to each raw line before preparation starts. To do it later, is much worse.
        text_lines = VALUE #(
            FOR _text_line_for_html IN text_module_lines
                  ( line = CONV #(       "If text line ends already with a line break <br> then keep it as is
                       COND #( WHEN _text_line_for_html-tdline CP |*{ tp_options->html_tag-line_break }|
                                 THEN _text_line_for_html-tdline
                                 ELSE |{ _text_line_for_html-tdline }{ tp_options->html_tag-line_break }| ) ) ) ).

      WHEN parent->tp_options->preparation_type-raw.
        text_lines = VALUE #( FOR _text_line_for_raw IN text_module_lines
                                    ( line = _text_line_for_raw-tdline ) ).
    ENDCASE.
  ENDMETHOD.                    "reconvert_into_txt_lines


  METHOD remove_unused_symbols.
    "-----------------------------------------------------------------*
    "   Remove unused symbols
    "-----------------------------------------------------------------*
    FIND ALL OCCURRENCES OF REGEX '#([A-Z]|[0-9])\w*\d*#'   "all letters incl. underline + digits starting + ending with #
                         IN TABLE text_lines IN CHARACTER MODE
                          RESULTS DATA(_matching_results).
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    "The results of the FIND statement is sorted by default by the columns LINE, OFFSET and LENGTH.
    "To avoid destruction of the text it is necessary to start with last result. Otherwise it comes to
    "a shift of the lines and at least the second result line would erase the wrong line.
    SORT _matching_results BY line   DESCENDING
                              offset DESCENDING.
    LOOP AT _matching_results REFERENCE INTO DATA(_matching_result).
      DATA(_text_line) = REF #( text_lines[ _matching_result->line ] ).
      IF strlen( _text_line->line ) EQ _matching_result->length + 4.   "Text line contains only that one symbol + <br>
        DELETE text_lines INDEX _matching_result->line.

      ELSE.
        _text_line->line = replace( val = _text_line->line  off = _matching_result->offset
                                                            len = _matching_result->length with = '?' ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.                    "remove_unused_symbols


  METHOD replace_sap_script_symbols.
    "-----------------------------------------------------------------*
    "   Replace SAP script symbols (= structured values)
    "-----------------------------------------------------------------*
    get_in_text_module_format( ).

    CALL FUNCTION 'Z_CA_REPLACE_TEXT_SYMBOLS'
      EXPORTING
        text_module_header  = text_module_header
        fields_n_structures = parent->fields_n_structures
      CHANGING
        text_module_lines   = text_module_lines.

    reconvert_into_txt_lines( ).
  ENDMETHOD.                    "replace_sap_script_symbols


  METHOD replace_symbol_by_value.
    "-----------------------------------------------------------------*
    "   Replace symbol by value in prepared text lines
    "-----------------------------------------------------------------*
    DATA(_symbol) = create_symbol_from_value_name( name ).
    REPLACE ALL OCCURRENCES OF _symbol IN TABLE text_lines
                          WITH value  IN CHARACTER MODE.
  ENDMETHOD.                    "replace_symbol_by_value


  METHOD set_text.
    "-----------------------------------------------------------------*
    "   Set text, e. g. after additional manipulation
    "-----------------------------------------------------------------*
    me->text_lines = text_lines.
  ENDMETHOD.                    "set_text

ENDCLASS.                     "zcl_ca_text_prepared_lines  IMPLEMENTATION


