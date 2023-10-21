"! <p class="shorttext synchronized" lang="en">CA-TBX: Preparation of a text</p>
"!
"! <p>This class replaces variables like SAP-Script forms does and also individual variables. These can
"! be internal tables, structures and special fields like links or simply single values.</p>
"!
"! <p>At first there is the classic text module as base of e. g. a e-mail. This has to be defined via
"! transaction SO10 and is the typical case. It is also possible to pass a text as a table that is persisted
"! somewhere else.</p>
"!
"! <p>Those texts are often enriched by standard SAP Script variables which are enclosed by ampersands (&).
"! The individual variables have to be enclosed by hashes (#) and are foremost used for tables and/or
"! links.</p>
"!
"! <p>Please be aware to have a look at the short description of the parameter fields as they offer some
"! important details how to use them, what is there default and also what is the format that has to be
"! passed, e. g. a padding value must be passed with a dot instead of a comma or the color has to be provided
"! with a # in front of the color hex code.</p>
CLASS zcl_ca_text_preparation DEFINITION PUBLIC
                                         ABSTRACT
                                         CREATE PROTECTED.

* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n t e r f a c e s
    INTERFACES:
      zif_ca_text_preparation.

*   a l i a s e s
    ALIASES:
*     Attributes
      boolean                        FOR zif_ca_text_preparation~boolean,
      sel_options                    FOR zif_ca_text_preparation~sel_options,
      tp_options                     FOR zif_ca_text_preparation~tp_options,
      text_in_preparation            FOR zif_ca_text_preparation~text_in_preparation,
      control_settings               FOR zif_ca_text_preparation~control_settings,
      fields_n_structures            FOR zif_ca_text_preparation~fields_n_structures,
      links                          FOR zif_ca_text_preparation~links,
      tables                         FOR zif_ca_text_preparation~tables,
      preparation_type               FOR zif_ca_text_preparation~preparation_type,

*     Methods
      get_result_as_stream           FOR zif_ca_text_preparation~get_result_as_stream,
      get_result_as_wi_descr_table   FOR zif_ca_text_preparation~get_result_as_wi_descr_table,
      get_result_as_sap_office_tab   FOR zif_ca_text_preparation~get_result_as_sap_office_tab,
      get_result_as_sap_script_table FOR zif_ca_text_preparation~get_result_as_sap_script_table,
      replace_n_transform            FOR zif_ca_text_preparation~replace_n_transform,
      use_text_from                  FOR zif_ca_text_preparation~use_text_from.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Get instance to corresponding preparation type</p>
      "!
      "! @parameter preparation_type        | <p class="shorttext synchronized" lang="en">Preparation type (see const ...=&gt;PREPARATION_TYPE-*)</p>
      "! @raising   zcx_ca_text_preparation | <p class="shorttext synchronized" lang="en">CA-TBX exception: While preparing text module</p>
      get_instance
        IMPORTING
          preparation_type TYPE zca_d_target_preparation_type DEFAULT zcl_ca_c_text_preparation=>preparation_type-html
        RETURNING
          VALUE(result)    TYPE REF TO zif_ca_text_preparation
        RAISING
          zcx_ca_text_preparation.

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
          zcx_ca_text_preparation.


  PROTECTED SECTION.
*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Are text lines already passed?</p>
      "!
      "! @raising   zcx_ca_text_preparation | <p class="shorttext synchronized" lang="en">CA-TBX exception: While preparing text module</p>
      are_text_lines_passed
        RAISING
          zcx_ca_text_preparation,

      "! <p class="shorttext synchronized" lang="en">Get text for hint that the result is a sample of test system</p>
      get_text_for_hint_of_a_sample FINAL
        RETURNING
          VALUE(result) TYPE string,

      "! <p class="shorttext synchronized" lang="en">Insert hint that text was created/mailed from a test system</p>
      insert_hint_for_test ABSTRACT,

      "! <p class="shorttext synchronized" lang="en">Is the hint for testing purposes required?</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">X = Yes, a hint for testing should be added</p>
      is_hint_for_test_required FINAL
        RETURNING
          VALUE(result) TYPE abap_boolean,

      "! <p class="shorttext synchronized" lang="en">Checks whether it is a productive system</p>
      is_productive_system FINAL
        RETURNING
          VALUE(result) TYPE abap_boolean,

      "! <p class="shorttext synchronized" lang="en">Remove unused symbols</p>
      "!
      "! @raising   zcx_ca_text_preparation | <p class="shorttext synchronized" lang="en">CA-TBX exception: While preparing text module</p>
      remove_unused_symbols
        RAISING
          zcx_ca_text_preparation,

      "! <p class="shorttext synchronized" lang="en">Replace link symbols</p>
      "!
      "! @raising   zcx_ca_text_preparation | <p class="shorttext synchronized" lang="en">CA-TBX exception: While preparing text module</p>
      replace_link_symbols
        RAISING
          zcx_ca_text_preparation,

      "! <p class="shorttext synchronized" lang="en">Replace SAP script symbols (= structured values)</p>
      "!
      "! <p>The replacement of SAP script symbols is implicitly the replacement of structured components. Therefore
      "! the method {@link .METH:REPLACE_SINGLE_SYMBOLS} replaces only elementary values.</p>
      "!
      "! <p>If you like to display several fields of a structure in one line of the text, then use parameter
      "! {@link ZIF_CA_TEXT_PREPARATION.METH:REPLACE_N_TRANSFORM.DATA:TABLES} of method
      "! {@link ZIF_CA_TEXT_PREPARATION.METH:REPLACE_N_TRANSFORM} and pass there a table with one row
      "! of content.</p>
      "!
      "! <p>This replacement has to happen in a function group or program because other objects, like e. g. classes,
      "! are not selectable in transaction SO10. Furthermore is it necessary to maintain the used DDIC table
      "! definitions in a different object than the main class. Otherwise it would not be possible to generate
      "! the changed code and use it immediately.</p>
      "!
      "! @raising   zcx_ca_text_preparation | <p class="shorttext synchronized" lang="en">CA-TBX exception: While preparing text module</p>
      replace_sap_script_symbols FINAL
        RAISING
          zcx_ca_text_preparation,

      "! <p class="shorttext synchronized" lang="en">Replace single symbols - replaces all occurrences</p>
      "!
      "! @raising   zcx_ca_text_preparation | <p class="shorttext synchronized" lang="en">CA-TBX exception: While preparing text module</p>
      replace_single_symbols
        RAISING
          zcx_ca_text_preparation,

      "! <p class="shorttext synchronized" lang="en">Replace table symbols - replaces first occurrence only</p>
      "!
      "! @raising   zcx_ca_text_preparation | <p class="shorttext synchronized" lang="en">CA-TBX exception: While preparing text module</p>
      replace_table_symbols
        RAISING
          zcx_ca_text_preparation,

      "! <p class="shorttext synchronized" lang="en">Provide defaults in control settings</p>
      "!
      "! @parameter control_settings        | <p class="shorttext synchronized" lang="en">Control settings for the text preparation</p>
      "! @raising   zcx_ca_text_preparation | <p class="shorttext synchronized" lang="en">CA-TBX exception: While preparing text module</p>
      set_defaults_in_ctrl_settings
        IMPORTING
          control_settings TYPE zca_s_text_prep_ctrl_settings
        RAISING
          zcx_ca_text_preparation.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Technical names for insertion of table definitions</p>
      BEGIN OF techn_name,
        function_group           TYPE syrepid VALUE 'SAPLZCA_TEXT_PREPARATION'  ##no_text,
        "! <p class="shorttext synchronized" lang="en">Name of include with DDIC defs. of FG ZCA_TEXT_PREPARATION</p>
        include_ddic_definitions TYPE syrepid VALUE 'LZCA_TEXT_PREPARATIONTAB'  ##no_text,
        "! <p class="shorttext synchronized" lang="en">Table SYST is default def. with a dot -&gt; always last one</p>
        table_syst               TYPE char10 VALUE 'SYST.' ##no_text,
      END   OF techn_name.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Technical description of a value</p>
      techn_descr         TYPE REF TO cl_abap_datadescr,

*     t a b l e s
      "! <p class="shorttext synchronized" lang="en">Source of FG include with DDIC table definitions</p>
      source_code_include TYPE seo_section_source,

*     s i n g l e   v a l u e s
      "! <p class="shorttext synchronized" lang="en">X = Source of include is locked</p>
      source_is_locked    TYPE abap_boolean VALUE abap_false.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Add new DDIC structures as TABLES definition into FG include</p>
      "!
      "! @raising   zcx_ca_text_preparation | <p class="shorttext synchronized" lang="en">CA-TBX exception: While preparing text module</p>
      add_new_ddic_defs_to_fg_incl
        RAISING
          zcx_ca_text_preparation,

      "! <p class="shorttext synchronized" lang="en">Add HTML addition for SAP script if not found or correct</p>
      add_html_additon_4_sap_script,

      "! <p class="shorttext synchronized" lang="en">Get index for insertion of the new DDIC structure</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">Results of FIND statement, including the table index</p>
      get_index_for_struct_insertion
        RETURNING
          VALUE(result) TYPE match_result,

      "! <p class="shorttext synchronized" lang="en">Checks whether the value is a relevant structure type</p>
      "!
      "! @parameter data_value              | <p class="shorttext synchronized" lang="en">Passed data value as reference</p>
      "! @parameter result                  | <p class="shorttext synchronized" lang="en">X = It is a relevant structure type</p>
      "! @raising   zcx_ca_text_preparation | <p class="shorttext synchronized" lang="en">CA-TBX exception: While preparing text module</p>
      is_a_relevant_structure_type
        IMPORTING
          data_value    TYPE REF TO data
        RETURNING
          VALUE(result) TYPE abap_boolean
        RAISING
          zcx_ca_text_preparation,

      "! <p class="shorttext synchronized" lang="en">Checks whether the structure is already defined or not</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">X = Requested structure is already defined</p>
      is_structure_already_defined
        RETURNING
          VALUE(result) TYPE abap_boolean,

      "! <p class="shorttext synchronized" lang="en">Lock include source to include new DDIC definitions</p>
      lock_source,

      "! <p class="shorttext synchronized" lang="en">Read source of FG include with DDIC table definitions</p>
      read_fg_include_with_ddic_defs,

      split_text_line_for_result
        IMPORTING
          text_line     TYPE REF TO zca_s_text_line
          split_size    TYPE i
        RETURNING
          VALUE(result) TYPE soli_tab,

      "! <p class="shorttext synchronized" lang="en">Release source object</p>
      unlock_source,

      "! <p class="shorttext synchronized" lang="en">Update changed include in DB + gen. FG and Where-Used-index</p>
      update_fg_include_and_generate.

ENDCLASS.



CLASS zcl_ca_text_preparation IMPLEMENTATION.

  METHOD add_html_additon_4_sap_script.
    "-----------------------------------------------------------------*
    "   Add HTML addition for SAP script if not found or correct
    "-----------------------------------------------------------------*
    FIND FIRST OCCURRENCE OF tp_options->techn_addition-mail_html_command
                                                        IN TABLE text_in_preparation->text_lines
                                                        MATCH LINE DATA(_found_in_line) ##no_text.
    IF sy-subrc NE 0.
      text_in_preparation->insert_line_at_the_beginning( CONV #( tp_options->techn_addition-mail_html_command ) ) ##no_text.

    ELSE.
      DATA(_line_with_sap_script_addition) = REF #( text_in_preparation->text_lines[ _found_in_line ] ).
      "Has a HTML tag added? -> Replace by the original.
      IF strlen( _line_with_sap_script_addition->line ) GT strlen( tp_options->techn_addition-mail_html_command ).
        _line_with_sap_script_addition->line = tp_options->techn_addition-mail_html_command.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "add_html_additon_4_sap_script


  METHOD add_new_ddic_defs_to_fg_incl.
    "-----------------------------------------------------------------*
    "   Add new DDIC structures as TABLES definition into FG include
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      _source_code_line TYPE seo_section_source_line.

    IF fields_n_structures IS INITIAL.
      RETURN.
    ENDIF.

    read_fg_include_with_ddic_defs( ).

    "Check structures
    LOOP AT fields_n_structures INTO  DATA(_field_or_structure)
                                WHERE value IS BOUND.
      IF NOT is_a_relevant_structure_type( _field_or_structure-value ) OR
             is_structure_already_defined( ).
        CONTINUE.
      ENDIF.

      "Try to lock the include source to avoid collision with another executions
      lock_source( ).

      "Find SYST table (predefined) to get index for insertion of the new DDIC structure
      DATA(_index_insertion) = get_index_for_struct_insertion( ).

      "Prepare new line with table definition and insert into coding. The spaces in front of the
      "first curly bracket are for indentation of the table statement.
      _source_code_line = |  { condense( to_lower( techn_descr->get_relative_name( ) ) ) } ##needed,| ##no_text.
      INSERT _source_code_line INTO  source_code_include
                              INDEX _index_insertion-line.
    ENDLOOP.
    IF source_is_locked EQ abap_false OR
       sy-subrc         NE 0.
      RETURN.      "Nothing changed or found - leave
    ENDIF.

    update_fg_include_and_generate( ).

    CLEAR: techn_descr,
           source_code_include,
           source_is_locked.
  ENDMETHOD.                    "add_new_ddic_defs_to_fg_incl


  METHOD are_text_lines_passed.
    "-----------------------------------------------------------------*
    "   Was method USE_TEXT_FROM executed before?
    "-----------------------------------------------------------------*
    IF text_in_preparation IS NOT BOUND.
      "Provide either a text module or text lines first before using preparation
      RAISE EXCEPTION TYPE zcx_ca_text_preparation
        EXPORTING
          textid   = zcx_ca_text_preparation=>no_text_available
          mv_msgty = zcx_ca_text_preparation=>c_msgty_e.
    ENDIF.

    text_in_preparation->are_text_lines_passed( ).
  ENDMETHOD.                    "are_text_lines_passed


  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*
    boolean     = zcl_ca_c_numeric_boolean=>get_instance( ).
    sel_options = zcl_ca_c_sel_options=>get_instance( ).
    tp_options  = zcl_ca_c_text_preparation=>get_instance( ).

    tp_options->is_preparation_type_valid( preparation_type ).
    me->preparation_type = preparation_type.
  ENDMETHOD.                    "constructor


  METHOD get_index_for_struct_insertion.
    "-----------------------------------------------------------------*
    "   Get index for insertion of the new DDIC structure
    "-----------------------------------------------------------------*
    FIND FIRST OCCURRENCE OF techn_name-table_syst IN TABLE source_code_include
                                                      IN CHARACTER MODE IGNORING CASE
                                                      RESULTS result.
    IF sy-subrc NE 0.
      "Predefined statement '&1' not found in source
      RAISE EXCEPTION TYPE zcx_ca_intern
        MESSAGE ID 'ZCA_TOOLBOX' TYPE 'E' NUMBER '028' WITH techn_name-table_syst.
    ENDIF.
  ENDMETHOD.                    "get_index_for_struct_insertion


  METHOD get_instance.
    "-----------------------------------------------------------------*
    "   Get instance for corresponding preparation type
    "-----------------------------------------------------------------*
    zcl_ca_c_text_preparation=>get_instance( )->is_preparation_type_valid( preparation_type ).

    TRY.
        DATA(_class_name) = |{ cl_oo_classname_service=>get_clsname_by_include( sy-repid ) }_{ preparation_type }|.
        CREATE OBJECT result TYPE (_class_name)
          EXPORTING
            preparation_type = preparation_type.

      CATCH cx_sy_create_object_error INTO DATA(_catched).
        DATA(_exception) = CAST zcx_ca_text_preparation(
                                      zcx_ca_error=>create_exception(
                                                   iv_excp_cls = zcx_ca_text_preparation=>c_zcx_ca_text_preparation
                                                   ix_error    = _catched ) ) ##no_text.
        IF _exception IS BOUND.
          RAISE EXCEPTION _exception.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "get_instance


  METHOD get_text_for_hint_of_a_sample.
    "-----------------------------------------------------------------*
    "   Get text for the hint that the result is a sample of test system
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      _textpool            TYPE STANDARD TABLE OF textpool.

    DATA(_logon_language) = cl_abap_syst=>get_logon_language( ).
    READ TEXTPOOL sy-repid INTO _textpool LANGUAGE _logon_language.
    IF sy-subrc NE 0 OR
       NOT line_exists( _textpool[ id  = 'I'
                                   key = 'MST' ] ) ##no_text.
      READ TEXTPOOL sy-repid INTO _textpool LANGUAGE 'E'.     "Read in English
    ENDIF.

    result = _textpool[ id  = 'I'
                        key = 'MST' ]-entry ##no_text.
  ENDMETHOD.                    "get_text_for_hint_of_a_sample


  METHOD is_a_relevant_structure_type.
    "-----------------------------------------------------------------*
    "   Checks whether the value is a relevant structure type
    "-----------------------------------------------------------------*
    "Get technical type description of value
    techn_descr = tp_options->get_technical_description( data_value ).

    "Respect only structures of flat type and defined in DDIC. Others can not be handled by SAP script.
    result = abap_true.
    IF techn_descr->type_kind       NE techn_descr->typekind_struct1 OR
       techn_descr->is_ddic_type( ) EQ abap_false.
      result = abap_false.
      RETURN.
    ENDIF.

    DATA(_struc_desc) = CAST cl_abap_structdescr( techn_descr ).
    IF _struc_desc->struct_kind EQ _struc_desc->structkind_nested OR
       _struc_desc->struct_kind EQ _struc_desc->structkind_mesh.
      result = abap_false.
    ENDIF.
  ENDMETHOD.                    "is_a_relevant_structure_type


  METHOD is_hint_for_test_required.
    "-----------------------------------------------------------------*
    "   Is the hint for testing purposes required?
    "-----------------------------------------------------------------*
    result = abap_true.
    IF control_settings-without_hint_test EQ boolean->true OR
       is_productive_system( ).
      result = abap_false.
    ENDIF.
  ENDMETHOD.                    "is_hint_for_test_required


  METHOD is_productive_system.
    "-----------------------------------------------------------------*
    "   Checks whether it is a productive system
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      _client_role          TYPE cccategory.

    " !!! BAdI für Kunden vorsehen, um individuelle Prüfung zu ermöglichen  !!!

    result = abap_false.
    CALL FUNCTION 'TR_SYS_PARAMS'
      IMPORTING
        system_client_role = _client_role.

    IF _client_role EQ 'P' ##no_text.
      result = abap_true.
    ENDIF.
  ENDMETHOD.                    "is_productive_system


  METHOD is_structure_already_defined.
    "-----------------------------------------------------------------*
    "   Checks whether the value is a relevant structure type
    "-----------------------------------------------------------------*
    result = abap_false.
    "Search in the source for DDIC definition in lower case
    FIND FIRST OCCURRENCE OF to_lower( techn_descr->get_relative_name( ) )
                    IN TABLE source_code_include IN CHARACTER MODE.
    IF sy-subrc EQ 0.
      result = abap_true.
    ENDIF.
  ENDMETHOD.                    "is_structure_already_defined


  METHOD lock_source.
    "-----------------------------------------------------------------*
    "   Lock include source to include new DDIC definitions
    "-----------------------------------------------------------------*
    IF source_is_locked EQ abap_true.
      RETURN.
    ENDIF.

    DATA(_is_locked) = abap_false.
    DO 5 TIMES.
      "Try to lock include
      CALL FUNCTION 'ENQUEUE_ESRDIRE'
        EXPORTING
          mode_trdir     = 'X'
          name           = techn_name-include_ddic_definitions
          _scope         = '1'
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.
      CASE sy-subrc.
        WHEN 0.
          source_is_locked = _is_locked = abap_true.
          EXIT.

        WHEN 1.
          WAIT UP TO 1 SECONDS.

        WHEN OTHERS.
          DATA(_exception) = CAST zcx_ca_intern( zcx_ca_intern=>create_exception(
                                                                   iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                                                   iv_function = 'ENQUEUE_ESRDIRE'
                                                                   iv_subrc    = sy-subrc ) ) ##no_text.
          IF _exception IS BOUND.
            RAISE EXCEPTION _exception.
          ENDIF.
      ENDCASE.
    ENDDO.

    IF _is_locked EQ abap_false.
      "Program &1 could not be locked - please try again later
      RAISE EXCEPTION TYPE zcx_ca_intern
        MESSAGE ID 'ZCA_TOOLBOX' TYPE 'E' NUMBER '026' WITH techn_name-include_ddic_definitions.
    ENDIF.
  ENDMETHOD.                    "lock_source


  METHOD read_fg_include_with_ddic_defs.
    "-----------------------------------------------------------------*
    "   Enhance TOP include of FG by DDIC definitions
    "-----------------------------------------------------------------*
    "Get source of include with DDIC definitions
    READ REPORT techn_name-include_ddic_definitions INTO source_code_include.
    IF sy-subrc NE 0.
      "Program &1 does not exist
      RAISE EXCEPTION TYPE zcx_ca_intern
        MESSAGE ID 'DS' TYPE 'E' NUMBER '017' WITH techn_name-include_ddic_definitions.
    ENDIF.
  ENDMETHOD.                    "read_fg_include_with_ddic_defs


  METHOD remove_unused_symbols.
    "-----------------------------------------------------------------*
    "   Remove unused symbols
    "-----------------------------------------------------------------*
    IF control_settings-remove_unused_symbols EQ boolean->false.
      RETURN.
    ENDIF.

    text_in_preparation->remove_unused_symbols( ).
  ENDMETHOD.                    "remove_unused_symbols


  METHOD replace_link_symbols.
    "-----------------------------------------------------------------*
    "   Replace link symbols
    "-----------------------------------------------------------------*
    "   h a s   t o   b e   r e d e f i n e d
  ENDMETHOD.                    "replace_link_symbols


  METHOD replace_sap_script_symbols.
    "-----------------------------------------------------------------*
    "   Replace SAP script symbols (= structured values)
    "-----------------------------------------------------------------*
    text_in_preparation->replace_sap_script_symbols( ).
  ENDMETHOD.                    "replace_sap_script_symbols


  METHOD replace_single_symbols.
    "-----------------------------------------------------------------*
    "   Replace single symbols - all occurrences are replaced
    "-----------------------------------------------------------------*
    LOOP AT fields_n_structures REFERENCE INTO DATA(_field_n_struct).
      TRY.
          IF NOT text_in_preparation->does_symbol_occur_in_text( _field_n_struct->name ).
            CONTINUE.
          ENDIF.

          "Get technical type description of value
          DATA(_element) = NEW zcl_ca_text_preparation_elem( name_to_value = CONV #( _field_n_struct->name )
                                                             value_ref     = _field_n_struct->value ).

          text_in_preparation->replace_symbol_by_value( name  = _field_n_struct->name
                                                        value = _element->convert_value_type_conform( ) ).

        CATCH zcx_ca_param INTO DATA(_catched) ##needed.     "Catching it for analytical purpose only
          CONTINUE.          "Ignore replacement in case of errors
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.                    "replace_single_symbols


  METHOD replace_table_symbols.
    "-----------------------------------------------------------------*
    "   Replace table symbols - only the first occurrence is replaced
    "-----------------------------------------------------------------*
    "Prepare each table and replace the corresponding symbol
    LOOP AT tables REFERENCE INTO DATA(_table_settings).
      TRY.
          IF NOT text_in_preparation->does_symbol_occur_in_text( _table_settings->name ).
            CONTINUE.
          ENDIF.

          DATA(_table) = zcl_ca_text_preparation_table=>get_instance( parent         = me
                                                                      table_settings = _table_settings ).

          text_in_preparation->exchange_symbol_by_table_rows( _table->prepare_table_rows( ) ).

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
  ENDMETHOD.                    "replace_table_symbols


  METHOD set_defaults_in_ctrl_settings.
    "-----------------------------------------------------------------*
    "   Provide defaults in control settings if necessary
    "-----------------------------------------------------------------*
    me->control_settings = control_settings.

    "General settings
    "Set with hint for output in test system
    IF me->control_settings-without_hint_test CN '01'.
      me->control_settings-without_hint_test = boolean->false.
    ENDIF.

    "Set default flag for removing unused symbols
    IF me->control_settings-remove_unused_symbols CN '01'.
      me->control_settings-remove_unused_symbols = boolean->true.
    ENDIF.
  ENDMETHOD.                    "set_defaults_in_ctrl_settings


  METHOD unlock_source.
    "-----------------------------------------------------------------*
    "   Release source object
    "-----------------------------------------------------------------*
    CALL FUNCTION 'DEQUEUE_ESRDIRE'
      EXPORTING
        mode_trdir = 'X'
        name       = techn_name-include_ddic_definitions
        _scope     = '1'.
  ENDMETHOD.                    "unlock_source


  METHOD update_fg_include_and_generate.
    "-----------------------------------------------------------------*
    "   Update source include in DB and generate FG and Where-Used-index
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      _generation_error_msg TYPE bapi_msg.

    "Insert changed source and generate it
    INSERT REPORT techn_name-include_ddic_definitions FROM source_code_include.
    GENERATE REPORT techn_name-function_group MESSAGE _generation_error_msg. "#EC CI_GENERATE
    IF sy-subrc NE 0.
      unlock_source( ).
      "Send message of generation
      RAISE EXCEPTION TYPE zcx_ca_intern
        MESSAGE ID 'S1' TYPE 'E' NUMBER '897' WITH
            _generation_error_msg(50) _generation_error_msg+50(50)
            _generation_error_msg+100(50) _generation_error_msg+150(50).
    ENDIF.

    "Actualize program index to provide 'SO10 - Maintenance text modules' with new DDIC structures.
    CALL FUNCTION 'WB_TREE_ACTUALIZE'
      EXPORTING
        tree_name = CONV seocpdname( |PG_{ techn_name-function_group }| ) ##no_text.

    CALL FUNCTION 'DB_COMMIT'.

    unlock_source( ).
  ENDMETHOD.                    "update_fg_include_and_generate


  METHOD split_text_line_for_result.
    "-----------------------------------------------------------------*
    "   Split a single line of prepared text into requested parts
    "-----------------------------------------------------------------*
    DATA(_text_line) = text_line->*.
    WHILE _text_line IS NOT INITIAL.
      APPEND VALUE #( line = _text_line(split_size) ) TO result.
      SHIFT _text_line BY split_size PLACES LEFT IN CHARACTER MODE.
    ENDWHILE.
  ENDMETHOD.                    "split_text_line_for_result


  METHOD zif_ca_text_preparation~get_result_as_stream.
    "-----------------------------------------------------------------*
    "   Get result as stream (= STRING)
    "-----------------------------------------------------------------*
    IF add_addition_if_missing EQ abap_true.
      add_html_additon_4_sap_script( ).
    ENDIF.

    result = concat_lines_of( table = text_in_preparation->text_lines ).
  ENDMETHOD.                    "zif_ca_text_preparation~get_result_as_stream


  METHOD zif_ca_text_preparation~get_result_as_wi_descr_table.
    "-----------------------------------------------------------------*
    "   Get result as workitem description table
    "-----------------------------------------------------------------*
    LOOP AT text_in_preparation->text_lines REFERENCE INTO DATA(_text_line).
      DATA(_lines_of_splitted_text_line) = split_text_line_for_result( text_line  = _text_line
                                                                       split_size = 132 ).
      APPEND LINES OF VALUE htmltable( FOR _split_line IN _lines_of_splitted_text_line
                                                   ( tdline = _split_line-line ) ) TO result.
    ENDLOOP.
  ENDMETHOD.                    "zif_ca_text_preparation~get_result_as_wi_descr_table


  METHOD zif_ca_text_preparation~get_result_as_sap_office_tab.
    "-----------------------------------------------------------------*
    "   Get result as SOLI_TAB (SAP office format, e. g. for mails)
    "-----------------------------------------------------------------*
    LOOP AT text_in_preparation->text_lines REFERENCE INTO DATA(_text_line).
      APPEND LINES OF split_text_line_for_result( text_line  = _text_line
                                                  split_size = 255 ) TO result.
    ENDLOOP.
  ENDMETHOD.                    "zif_ca_text_preparation~get_result_as_sap_office_tab


  METHOD zif_ca_text_preparation~get_result_as_sap_script_table.
    "-----------------------------------------------------------------*
    "   Get result as SAP script module table (TLINE_TAB)
    "-----------------------------------------------------------------*
    LOOP AT text_in_preparation->text_lines REFERENCE INTO DATA(_text_line).
      DATA(_lines_of_splitted_text_line) = split_text_line_for_result( text_line  = _text_line
                                                                       split_size = 132 ).
      APPEND LINES OF VALUE tline_tab( FOR _split_line IN _lines_of_splitted_text_line
                                                   ( tdformat = '/'
                                                     tdline   = _split_line-line ) ) TO result.
    ENDLOOP.
  ENDMETHOD.                    "zif_ca_text_preparation~get_result_as_sap_script_table


  METHOD zif_ca_text_preparation~replace_n_transform.
    "-----------------------------------------------------------------*
    "   Replace variables and transform text module into target format
    "-----------------------------------------------------------------*
    TRY.
        "Is this method called in wrong order? Method USE_TEXT_FROM has to be called before this one.
        are_text_lines_passed( ).

        "Keep importing parameters in class attributes
        set_defaults_in_ctrl_settings( control_settings ).
        me->fields_n_structures = fields_n_structures.
        me->tables              = tables.
        me->links               = links.

        "Enhance specific include of function group with new DDIC structures for usage in transaction SO10
        add_new_ddic_defs_to_fg_incl( ).   "<==  This method enhance the include LZCA_TEXT_PREPARATIONTAB of
        "FG ZCA_TEXT_PREPARATION, which is part of the TOP include, to create data definitions as global fields
        "of the corresponding DDIC type, e. g. GS_EKKO type EKKO, if the column NAME has a different value to
        "the technical type.

        "At first do the SAP script replacements
        replace_sap_script_symbols( ).                 "= replaces symbols from structured data objects
        "<==  Enhance subroutine SET_INBOUND_DATA_INTO_GLOBAL to provide
        "also the global fields mentioned above.

        "Insert the note only now to avoid inserting it into different source tables
        insert_hint_for_test( ).
        replace_single_symbols( ).
        replace_table_symbols( ).
        replace_link_symbols( ).
        remove_unused_symbols( ).

      CATCH zcx_ca_error
            zcx_ca_intern INTO DATA(_catched).
        DATA(_exception) = CAST zcx_ca_text_preparation(
                                   zcx_ca_error=>create_exception(
                                                 iv_excp_cls = zcx_ca_text_preparation=>c_zcx_ca_text_preparation
                                                 ix_error    = _catched ) ) ##no_text.
        IF _exception IS BOUND.
          RAISE EXCEPTION _exception.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "zif_ca_text_preparation~replace_n_transform


  METHOD zif_ca_text_preparation~use_text_from.
    "-----------------------------------------------------------------*
    "   Import text from table or text module (the later = SO10)
    "-----------------------------------------------------------------*
    text_in_preparation = NEW #( parent          = me
                                 text_module_key = text_module_key
                                 text_lines      = text_lines ).
  ENDMETHOD.                    "zif_ca_text_preparation~use_text_from

ENDCLASS.
