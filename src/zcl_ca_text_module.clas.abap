"! <p class="shorttext synchronized" lang="en">CA-TBX: Preparation of a text module</p>
CLASS zcl_ca_text_module DEFINITION PUBLIC
                                    FINAL
                                    CREATE PUBLIC.

* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n t e r f a c e s
    INTERFACES:
      if_fsbp_const_range,
      zif_ca_c_bool,
      zif_ca_c_text_modules.

*   a l i a s e s
    ALIASES:
*     Boolean flags
      c_false                FOR  zif_ca_c_bool~c_false,
      c_true                 FOR  zif_ca_c_bool~c_true,
*     Signs and options for RANGES/SELECT-OPTIONS
      c_sign_i               FOR  if_fsbp_const_range~sign_include,
      c_sign_e               FOR  if_fsbp_const_range~sign_exclude,
*     Document classes, font, font types and sizes
      c_docclass_htm         FOR  zif_ca_c_text_modules~c_docclass_htm,
      c_docclass_raw         FOR  zif_ca_c_text_modules~c_docclass_raw,

      c_def_fontsize_monospc FOR  zif_ca_c_text_modules~c_def_fontsize_monospc,
      c_def_fontsize_prop    FOR  zif_ca_c_text_modules~c_def_fontsize_prop,

      c_def_font_monospc     FOR  zif_ca_c_text_modules~c_def_font_monospc,
      c_def_font_prop        FOR  zif_ca_c_text_modules~c_def_font_prop,

      c_fonttype_monospc     FOR  zif_ca_c_text_modules~c_fonttype_monospc,
      c_fonttype_prop        FOR  zif_ca_c_text_modules~c_fonttype_prop,

      c_table_desc_alignm_center  FOR  zif_ca_c_text_modules~c_table_desc_alignm_center,
      c_table_desc_alignm_left    FOR  zif_ca_c_text_modules~c_table_desc_alignm_left,
      c_table_desc_alignm_right   FOR  zif_ca_c_text_modules~c_table_desc_alignm_right,
      c_table_desc_alignm_inherit FOR  zif_ca_c_text_modules~c_table_desc_alignm_inherit,

      c_framestyle_none      FOR  zif_ca_c_text_modules~c_framestyle_none,
      c_framestyle_solid     FOR  zif_ca_c_text_modules~c_framestyle_solid,
      c_framestyle_ridge     FOR  zif_ca_c_text_modules~c_framestyle_ridge,
      c_framestyle_inherit   FOR  zif_ca_c_text_modules~c_framestyle_inherit,

      c_framewidth_thin      FOR  zif_ca_c_text_modules~c_framewidth_thin,
      c_framewidth_medium    FOR  zif_ca_c_text_modules~c_framewidth_medium,
      c_framewidth_thick     FOR  zif_ca_c_text_modules~c_framewidth_thick,
      c_framewidth_inherit   FOR  zif_ca_c_text_modules~c_framewidth_inherit.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Get text module and replace symbols</p>
      "!
      "! @raising   zcx_ca_text_module | <p class="shorttext synchronized" lang="en">Common exception: While preparing text module</p>
      get_text_module_n_replace
        IMPORTING
          is_text_key   TYPE stxh_key                OPTIONAL
          it_text       TYPE tline_tab               OPTIONAL
          it_flds_strcs TYPE zca_tt_params           OPTIONAL
          it_tables     TYPE zca_tt_tables_in_text   OPTIONAL
          it_links      TYPE zca_tt_texts_n_links    OPTIONAL
          is_text_font  TYPE zca_s_text_n_font_types OPTIONAL
        EXPORTING
          et_text       TYPE tline_tab
          et_mail_text  TYPE soli_tab
        RAISING
          zcx_ca_text_module,

      "! <p class="shorttext synchronized" lang="en">Convert SOLI_TAB into HTML/TEXT stream for Fiori WF descr.</p>
      "!
      "! <p>This method converts the result of method GET_TEXT_MODULE_N_REPLACE into a format that is expected
      "! by the enhancement spot /IWPGW/ES_TGW_TASK_DATA method MODIFY_TASK_DESCRIPTION parameter CV_DESCRIPTION_HTML.</p>
      "!
      "! @parameter it_details | <p class="shorttext synchronized" lang="en">Details in SOLI_TAB</p>
      "! @parameter result     | <p class="shorttext synchronized" lang="en">Details in TDLINE for BO table element</p>
      convert_soli_tab_2_html_stream
        IMPORTING
          it_details    TYPE soli_tab
        RETURNING
          VALUE(result) TYPE /iwpgw/tgw_task_description_ht,

      "! <p class="shorttext synchronized" lang="en">Convert SOLI_TAB into TDLINE table for WF description in BO</p>
      "!
      "! <p>This method converts the result of method GET_TEXT_MODULE_N_REPLACE into a format that can be
      "! used as a table attribute in classic Business Objects (BO), e. g. to provide the task description
      "! with tables as output element, such like a approver list or purchase order items to be approved.</p>
      "!
      "! @parameter it_details | <p class="shorttext synchronized" lang="en">Details in SOLI_TAB</p>
      "! @parameter result     | <p class="shorttext synchronized" lang="en">Details in TDLINE for BO table element</p>
      convert_soli_tab_2_html_table
        IMPORTING
          it_details    TYPE soli_tab
        RETURNING
          VALUE(result) TYPE htmltable.


  PROTECTED SECTION.
*   l o c a l   t y p e   d e f i n i t i o n
    TYPES:
      "! <p class="shorttext synchronized" lang="en">Combined structure of component and DDIC description</p>
      BEGIN OF ty_s_stru_comp,
        s_comp  TYPE abap_simple_componentdescr,
        s_dfies TYPE dfies,
      END   OF ty_s_stru_comp,
      "! <p class="shorttext synchronized" lang="en">Table with combined data description</p>
      ty_t_stru_comps TYPE STANDARD TABLE OF ty_s_stru_comp
                                    WITH KEY s_comp-name.

*   a l i a s e s
    ALIASES:
*     Signs and options for RANGES/SELECT-OPTIONS
      c_opt_eq               FOR  if_fsbp_const_range~option_equal,
      c_opt_ne               FOR  if_fsbp_const_range~option_not_equal,
      c_opt_cp               FOR  if_fsbp_const_range~option_contains_pattern,
      c_opt_np               FOR  if_fsbp_const_range~option_not_contains_pattern,
      c_opt_bt               FOR  if_fsbp_const_range~option_between,
      c_opt_nb               FOR  if_fsbp_const_range~option_not_between.

*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">HTML tags</p>
      BEGIN OF cs_css_element,
        border          TYPE its_tag VALUE '{ border:'  ##no_text,
        border_collapse TYPE its_tag VALUE 'border-collapse: collapse }'  ##no_text,
      END   OF cs_css_element,

      "! <p class="shorttext synchronized" lang="en">HTML tags</p>
      BEGIN OF cs_htmltag,
        close_body         TYPE its_tag VALUE '</body>'  ##no_text,
        close_div          TYPE its_tag VALUE '</div>'  ##no_text,
        close_font         TYPE its_tag VALUE '</font>'  ##no_text,
        close_head         TYPE its_tag VALUE '</head>'  ##no_text,
        close_html         TYPE its_tag VALUE '</html>'  ##no_text,
        close_table        TYPE its_tag VALUE '</table>'  ##no_text,
        close_tab_body     TYPE its_tag VALUE '</tbody>'  ##no_text,
        close_tab_hdr      TYPE its_tag VALUE '</thead>'  ##no_text,
        close_tab_data_col TYPE its_tag VALUE '</td>'  ##no_text,
        close_tab_hdr_col  TYPE its_tag VALUE '</th>'  ##no_text,
        close_tab_row      TYPE its_tag VALUE '</tr>'  ##no_text,
*        head_meta_settings TYPE its_tag VALUE '<meta http-equiv="content-type" content="text/html; charset=&charset&">'  ##no_text,
*        intro_1            TYPE its_tag VALUE '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 TRANSITIONAL//EN"'  ##no_text,
*        intro_2            TYPE its_tag VALUE '       "http://www.w3.org/TR/html4/strict.dtd">'  ##no_text,
        line_break         TYPE its_tag VALUE '<br>'  ##no_text,
        open_body          TYPE its_tag VALUE '<body>'  ##no_text,
*        open_div           TYPE its_tag VALUE '<div>'  ##no_text,
        open_font          TYPE its_tag VALUE '<font face="&1,&2" size="&3">'  ##no_text,
        open_head          TYPE its_tag VALUE '<head>'  ##no_text,
        open_html          TYPE its_tag VALUE '<html>'  ##no_text,
        open_table         TYPE its_tag VALUE '<table>'  ##no_text,
        open_tab_body      TYPE its_tag VALUE '<tbody>'  ##no_text,
        open_tab_hdr       TYPE its_tag VALUE '<thead>'  ##no_text,
        open_tab_data_col  TYPE its_tag VALUE '<td>'  ##no_text,
        open_tab_hdr_col   TYPE its_tag VALUE '<th>'  ##no_text,
        open_tab_row       TYPE its_tag VALUE '<tr>'  ##no_text,
      END   OF cs_htmltag,

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

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     t a b l e s
      "! <p class="shorttext synchronized" lang="en">Range table with excluding or including output fields</p>
      mra_output_flds TYPE rsdsselopt_t,

*     s t r u c t u r e s
      "! <p class="shorttext synchronized" lang="en">Common object: Text and font types for output as mail</p>
      ms_text_font    TYPE zca_s_text_n_font_types,

*     s i n g l e   v a l u e s
      "! <p class="shorttext synchronized" lang="en">1 = Text will be prepared for a mail - formatting in HTML</p>
      mv_is_mailtext  TYPE abap_bool.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Attach column value to output line, incl. tags / tabulator</p>
      attach_column_to_output_line
        IMPORTING
          iv_fld_output  TYPE clike
          iv_fld_len     TYPE i
          is_table       TYPE zca_s_table_in_text
          iv_txt_fld_len TYPE i
          iv_is_header   TYPE abap_bool DEFAULT abap_false
        CHANGING
          cv_txt_fld     TYPE clike
          cv_pos         TYPE i,

      "! <p class="shorttext synchronized" lang="en">Set default values for non-provided table settings</p>
      complete_table_defaults
        IMPORTING
          is_table        TYPE zca_s_table_in_text
        RETURNING
          VALUE(rs_table) TYPE zca_s_table_in_text,

      "! <p class="shorttext synchronized" lang="en">Convert (SAP-Script) text into mail text</p>
      convert_text_2_mail_text
        IMPORTING
          it_text             TYPE tline_tab
        RETURNING
          VALUE(rt_mail_text) TYPE soli_tab,

      "! <p class="shorttext synchronized" lang="en">Create symbols for search (name bracketed in hashes)</p>
      create_symbol
        IMPORTING
          iv_value_name    TYPE rs38l_par_
        RETURNING
          VALUE(rv_symbol) TYPE string,

      "! <p class="shorttext synchronized" lang="en">Create style addition for the tags 'table', 'th' and 'td'</p>
      create_style_for_table_n_cells
        IMPORTING
          is_table                 TYPE zca_s_table_in_text
        RETURNING
          VALUE(rv_style_addition) TYPE string,

      "! <p class="shorttext synchronized" lang="en">Determine text field - is it text module or mail length</p>
      determine_text_field
        IMPORTING
          ir_text_line   TYPE REF TO data
        EXPORTING
          ev_txt_fld_len TYPE i
          er_txt_fld     TYPE REF TO data
        CHANGING
          cs_table       TYPE zca_s_table_in_text OPTIONAL,

      "! <p class="shorttext synchronized" lang="en">Enhance style for cell tags 'td' to align the value right</p>
      enhance_cell_style_align_right
        IMPORTING
          is_table                 TYPE zca_s_table_in_text
        RETURNING
          VALUE(rv_style_addition) TYPE string,

      "! <p class="shorttext synchronized" lang="en">Get text module</p>
      "!
      "! @raising   zcx_ca_text_module | <p class="shorttext synchronized" lang="en">Common exception: While preparing text module</p>
      get_text_module
        IMPORTING
          is_text_key TYPE stxh_key
        EXPORTING
          et_text     TYPE tline_tab
          es_thead    TYPE thead
        RAISING
          zcx_ca_text_module,

      "! <p class="shorttext synchronized" lang="en">Insert hint that text was created/mailed from a test system</p>
      insert_hint_for_test
        CHANGING
          ct_text TYPE tline_tab,

      "! <p class="shorttext synchronized" lang="en">Output of a single column</p>
      "!
      "! @raising   zcx_ca_conv | <p class="shorttext synchronized" lang="en">Common exception: Conversion failed</p>
      output_single_column
        IMPORTING
          is_table       TYPE zca_s_table_in_text
          is_output_fld  TYPE zca_s_output_field OPTIONAL
          is_stru_comp   TYPE ty_s_stru_comp
          iv_txt_fld_len TYPE i
          is_data_line   TYPE data
        CHANGING
          cv_txt_fld     TYPE clike
          cv_pos         TYPE i
        RAISING
          zcx_ca_conv,

      "! <p class="shorttext synchronized" lang="en">Output of a single column header</p>
      "!
      "! @raising   zcx_ca_text_module | <p class="shorttext synchronized" lang="en">Common exception: While preparing text module</p>
      create_table_header
        IMPORTING
          it_stru_comps  TYPE ty_t_stru_comps
          is_table       TYPE zca_s_table_in_text
          iv_txt_fld_len TYPE i
        CHANGING
          cv_txt_fld     TYPE clike
          cv_pos         TYPE i
        RAISING
          zcx_ca_text_module,

      "! <p class="shorttext synchronized" lang="en">Output of a single column header</p>
      "!
      "! @raising   zcx_ca_conv        | <p class="shorttext synchronized" lang="en">Common exception: Conversion failed</p>
      "! @raising   zcx_ca_text_module | <p class="shorttext synchronized" lang="en">Common exception: While preparing text module</p>
      create_table_line
        IMPORTING
          is_data_line   TYPE data
          it_stru_comps  TYPE ty_t_stru_comps
          is_table       TYPE zca_s_table_in_text
          iv_txt_fld_len TYPE i
        CHANGING
          cv_txt_fld     TYPE clike
          cv_pos         TYPE i
        RAISING
          zcx_ca_conv
          zcx_ca_text_module,

      "! <p class="shorttext synchronized" lang="en">Output of a single column header</p>
      "!
      "! @raising   zcx_ca_text_module | <p class="shorttext synchronized" lang="en">Common exception: While preparing text module</p>
      output_single_column_header
        IMPORTING
          is_table       TYPE zca_s_table_in_text
          is_stru_comp   TYPE ty_s_stru_comp
          iv_txt_fld_len TYPE i
        CHANGING
          cv_txt_fld     TYPE clike
          cv_pos         TYPE i
        RAISING
          zcx_ca_text_module,

      "! <p class="shorttext synchronized" lang="en">Prepare link lines - Description what to do and the link</p>
      "!
      "! @raising   zcx_ca_text_module | <p class="shorttext synchronized" lang="en">Common exception: While preparing text module</p>
      prepare_link_lines
        IMPORTING
          is_link       TYPE zca_s_text_n_link
        EXPORTING
          et_link_lines TYPE STANDARD TABLE
        RAISING
          zcx_ca_text_module,

      "! <p class="shorttext synchronized" lang="en">Prepare table data (= output)</p>
      "!
      "! @raising   zcx_ca_text_module | <p class="shorttext synchronized" lang="en">Common exception: While preparing text module</p>
      "! @raising   zcx_ca_conv        | <p class="shorttext synchronized" lang="en">Common exception: Conversion failed</p>
      prepare_table_lines
        EXPORTING
          et_text  TYPE STANDARD TABLE
        CHANGING
          cs_table TYPE zca_s_table_in_text
        RAISING
          zcx_ca_text_module
          zcx_ca_conv,

      "! <p class="shorttext synchronized" lang="en">Replace link symbols</p>
      provide_cell_with_style
        IMPORTING
          iv_mail_line           TYPE csequence
          iv_style_table_n_cells TYPE string
        RETURNING
          VALUE(rv_mail_line)    TYPE so_text255,

      "! <p class="shorttext synchronized" lang="en">Replace link symbols</p>
      "!
      "! @raising   zcx_ca_text_module | <p class="shorttext synchronized" lang="en">Common exception: While preparing text module</p>
      replace_link_symbols
        IMPORTING
          it_links TYPE zca_tt_texts_n_links
        CHANGING
          ct_text  TYPE STANDARD TABLE
        RAISING
          zcx_ca_text_module,

      "! <p class="shorttext synchronized" lang="en">Replace single symbols /fields</p>
      replace_single_symbols
        IMPORTING
          it_flds_strcs TYPE zca_tt_params
        CHANGING
          ct_text       TYPE STANDARD TABLE,

      "! <p class="shorttext synchronized" lang="en">Replace table symbols by output of requested table data</p>
      "!
      "! @raising   zcx_ca_text_module | <p class="shorttext synchronized" lang="en">Common exception: While preparing text module</p>
      "! @raising   zcx_ca_conv        | <p class="shorttext synchronized" lang="en">Common exception: Conversion failed</p>
      replace_table_symbols
        IMPORTING
          it_tables TYPE zca_tt_tables_in_text
        CHANGING
          ct_text   TYPE STANDARD TABLE
        RAISING
          zcx_ca_text_module
          zcx_ca_conv,

      "! <p class="shorttext synchronized" lang="en">Enclose current mail lines with font tags -> face/font size</p>
      set_html_tag_font
        IMPORTING
          iv_use_prop_font TYPE dml_boolean DEFAULT c_true
*          iv_close_last    TYPE abap_bool DEFAULT abap_true
        CHANGING
          ct_mail_text     TYPE soli_tab,

      "! <p class="shorttext synchronized" lang="en">Set tags around links that they recognized as links</p>
      set_html_tag_for_link
        IMPORTING
          iv_name        TYPE zca_d_name_link OPTIONAL
          iv_link        TYPE zca_d_link
        RETURNING
          VALUE(rv_link) TYPE zca_d_link,

      "! <p class="shorttext synchronized" lang="en">Enclose completed mail in tags for HTML declaration and body</p>
      set_html_tag_intro_n_body
        IMPORTING
          iv_set_prop_font TYPE abap_bool DEFAULT abap_true
          it_tables        TYPE zca_tt_tables_in_text
        CHANGING
          ct_mail_text     TYPE soli_tab,

      "! <p class="shorttext synchronized" lang="en">Set line break tag at end of each text line</p>
      set_html_tag_line_break
        CHANGING
          ct_mail_text TYPE soli_tab,

      "! <p class="shorttext synchronized" lang="en">Enclose table data with table and table record tags</p>
      set_html_tag_table
        IMPORTING
          is_table     TYPE zca_s_table_in_text
        CHANGING
          ct_mail_text TYPE STANDARD TABLE,

      "! <p class="shorttext synchronized" lang="en">Set text type, font and font size</p>
      set_text_type_n_font
        IMPORTING
          is_text_font TYPE zca_s_text_n_font_types.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Program name of function group with include below</p>
      c_name_function_grp  TYPE syrepid VALUE 'SAPLZCA_TEXT_MODULES'  ##no_text,
      "! <p class="shorttext synchronized" lang="en">Name of include with DDIC definitions of FG ZCA_TEXT_MODULES</p>
      c_name_incl_ddic_def TYPE syrepid VALUE 'LZCA_TEXT_MODULESTAB'  ##no_text.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Check FP for new structures and generate them</p>
      "!
      "! @parameter it_flds_strcs      | <p class="shorttext synchronized" lang="en">Common object: Value pairs - Name and value reference</p>
      "! @raising   zcx_ca_text_module | <p class="shorttext synchronized" lang="en">Common exception: While preparing text module</p>
      check_n_generate_function_pool
        IMPORTING
          it_flds_strcs TYPE zca_tt_params
        RAISING
          zcx_ca_text_module,

      "! <p class="shorttext synchronized" lang="en">Lock report souce</p>
      "!
      "! @raising   zcx_ca_text_module | <p class="shorttext synchronized" lang="en">Common exception: While preparing text module</p>
      lock_source
        RAISING
          zcx_ca_text_module,

      "! <p class="shorttext synchronized" lang="en">Release source object</p>
      unlock_source.
ENDCLASS.



CLASS zcl_ca_text_module IMPLEMENTATION.

  METHOD attach_column_to_output_line.
    "-----------------------------------------------------------------*
    "   Attach column value to output line, incl. tags / tabulator
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lv_new_pos TYPE syfdpos,        "New pos
      lv_xpos    TYPE syfdpos,        "Position of hex value
      lv_fld_len TYPE syfdpos.        "Output or real length

    FIELD-SYMBOLS:
      <lv_xval>            TYPE data.           "for conversion in hex for tabulator

    "For RAW output the length is important to create a table-like output;
    "while in HTML it is not so important, because the values will be
    "justified by the table tags. So than we can use the real length.
    CASE ms_text_font-doc_class.
      WHEN c_docclass_raw.
        "S A P   o f f i c e   t e x t
        lv_fld_len = iv_fld_len.

      WHEN c_docclass_htm.
        "H T M L   t e x t
        lv_fld_len = strlen( iv_fld_output ).
        IF lv_fld_len LE 0.
          lv_fld_len = 1.
        ENDIF.
    ENDCASE.

    "Check if value can be attached
    lv_new_pos = cv_pos + lv_fld_len.
    IF lv_new_pos GE iv_txt_fld_len.
      cv_pos = c_length_too_long.
      RETURN.
    ENDIF.

    "Attach value to line and increase offset
    cv_txt_fld+cv_pos(lv_fld_len) = iv_fld_output.
    cv_pos = cv_pos + lv_fld_len.

    "Calculate next position respecting column distance and/or delimiter
    CASE is_table-tab_delimited.
      WHEN c_false.
        "n o   t a b u l a t o r , but space or a delimiter if set
        "Set only space(s) between columns
        IF is_table-delimiter IS INITIAL.
          cv_pos = cv_pos + is_table-col_distance.
          IF cv_pos GE iv_txt_fld_len.
            cv_pos = c_length_too_long.
            RETURN.
          ENDIF.

        ELSE.
          "Is a delimiter requested?
          lv_new_pos = cv_pos + 1.
          IF lv_new_pos GE iv_txt_fld_len.
            cv_pos = c_length_too_long.
            RETURN.
          ENDIF.

          "Set delimiter
          cv_txt_fld+cv_pos(1) = is_table-delimiter.
          "Increase position by one for the next value
          cv_pos = cv_pos + 1.
        ENDIF.

      WHEN c_true.
        "t a b u l a t o r  requested
        "In case of using tabulator the column distance is fix 1 byte
        CASE ms_text_font-doc_class.
          WHEN c_docclass_raw.
            "S A P   o f f i c e   t e x t
            "All EVN systems are in unicode - so one character = 2 byte binary
            lv_xpos = cv_pos * cl_abap_char_utilities=>charsize.
            ASSIGN cv_txt_fld TO <lv_xval> CASTING TYPE x.
            <lv_xval>+lv_xpos(2) = c_coldelim_tabulator.

          WHEN c_docclass_htm.
            "H T M L   t e x t
            cv_txt_fld+cv_pos(1) = c_coldelim_hash.
        ENDCASE.
        "Increase position by one for the next value
        cv_pos = cv_pos + 1.
    ENDCASE.

    IF cv_pos GE iv_txt_fld_len.
      cv_pos = c_length_too_long.
    ENDIF.
  ENDMETHOD.                    "attach_column_to_output_line


  METHOD check_n_generate_function_pool.
    "-----------------------------------------------------------------*
    "   Check FP for new structures and generate them
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lo_type_desc    TYPE REF TO cl_abap_typedescr,
      lt_src_def      TYPE seo_section_source,  "Source code with table definitions
      ls_src_def      TYPE seo_section_source_line,
      ls_flds_strcs   TYPE zca_s_param,
      ls_match_result TYPE match_result,
      lv_is_locked    TYPE abap_bool       VALUE abap_false,  "1 = is locked by current exec
      lv_gen_msg      TYPE bapi_msg,
      lv_tree_name    TYPE seocpdname.

    "Leave, if no data transferred
    IF it_flds_strcs IS INITIAL.
      RETURN.
    ENDIF.

    "Get source of include with DDIC definitions
    READ REPORT c_name_incl_ddic_def INTO lt_src_def.
    IF sy-subrc NE 0.
      sy-msgv1 = c_name_incl_ddic_def.
      "Program &1 does not exist
      RAISE EXCEPTION TYPE zcx_ca_text_module
        EXPORTING
          textid   = zcx_ca_text_module=>source_not_found
          mv_msgv1 = sy-msgv1.
    ENDIF.

    "Check structures
    LOOP AT it_flds_strcs INTO  ls_flds_strcs
                          WHERE value IS BOUND.
      "Get name of inbound value if it is not set
      lo_type_desc = cl_abap_structdescr=>describe_by_data_ref( ls_flds_strcs-value ).
      "Respect only structures of flat type and defined in DDIC
      IF lo_type_desc->type_kind       NE lo_type_desc->typekind_struct1 OR
         lo_type_desc->is_ddic_type( ) EQ abap_false.
        CONTINUE.
      ENDIF.

      DATA(lo_struc_desc) = CAST cl_abap_structdescr( lo_type_desc ).
      IF lo_struc_desc->struct_kind EQ lo_struc_desc->structkind_nested OR
         lo_struc_desc->struct_kind EQ lo_struc_desc->structkind_mesh.
        CONTINUE.
      ENDIF.

      ls_flds_strcs-name = lo_type_desc->get_relative_name( ).

      "Search for DDIC definition in source
      TRANSLATE ls_flds_strcs-name TO LOWER CASE.
      FIND FIRST OCCURRENCE OF ls_flds_strcs-name
                      IN TABLE lt_src_def IN CHARACTER MODE
                       RESULTS ls_match_result.
      IF sy-subrc EQ 0.
        "If DDIC definition was found nothing else is to do - search for next
        CONTINUE.
      ENDIF.

      "Try to lock source to avoid collision with another execution
      IF lv_is_locked EQ abap_false.
        lock_source( ).
        lv_is_locked = abap_true.
      ENDIF.

      "Find SYST table (predefined) for inserting new table
      FIND FIRST OCCURRENCE OF 'SYST.' ##no_text
                      IN TABLE lt_src_def IN CHARACTER MODE
                                          IGNORING CASE
                       RESULTS ls_match_result.
      IF sy-subrc NE 0.
        sy-msgv1 = 'SYST.' ##no_text.
        "Predefined statement '&1' not found in source
        RAISE EXCEPTION TYPE zcx_ca_text_module
          EXPORTING
            textid   = zcx_ca_text_module=>src_strg_not_found
            mv_msgv1 = sy-msgv1.
      ENDIF.

      "Prepare new line with table definition and insert into coding
      CONCATENATE ls_flds_strcs-name
                  ','           INTO ls_src_def ##no_text.
      SHIFT ls_src_def RIGHT BY 2 PLACES.
      ls_src_def+30 = '"#EC NEEDED' ##no_text.
      INSERT ls_src_def INTO  lt_src_def
                        INDEX ls_match_result-line.
    ENDLOOP.
    IF sy-subrc     NE 0       OR
       lv_is_locked EQ abap_false.
      "Nothing changed - leave
      RETURN.
    ENDIF.

    "Insert changed source and generate it
    INSERT REPORT c_name_incl_ddic_def FROM lt_src_def.
    GENERATE REPORT c_name_function_grp MESSAGE lv_gen_msg. "#EC CI_GENERATE
    IF sy-subrc NE 0.
      unlock_source( ).
      "Send message of generation
      RAISE EXCEPTION TYPE zcx_ca_text_module
        EXPORTING
          textid   = zcx_ca_text_module=>any_other_msg
          mv_msgv1 = lv_gen_msg(50)
          mv_msgv2 = lv_gen_msg+50(50)
          mv_msgv3 = lv_gen_msg+100(50)
          mv_msgv4 = lv_gen_msg+150(50).
    ENDIF.

    "Actualize program index to provide "SO10 - Maintenance text modules"
    "with new DDIC structures.
    CONCATENATE 'PG_' c_name_function_grp INTO lv_tree_name ##no_text.
    CALL FUNCTION 'WB_TREE_ACTUALIZE'
      EXPORTING
        tree_name = lv_tree_name.

    CALL FUNCTION 'DB_COMMIT'.

    "Release source
    unlock_source( ).
  ENDMETHOD.                    "check_n_generate_function_pool


  METHOD complete_table_defaults.
    "-----------------------------------------------------------------*
    "   Set default values for non-provided table settings
    "-----------------------------------------------------------------*
    rs_table = is_table.
    "Set defaults if no values are set
    IF rs_table-sign_for_flds CN 'IE' ##no_text.
      rs_table-sign_for_flds = c_sign_i.
    ENDIF.

    IF rs_table-tab_delimited CN '01'            OR   "only 0 (= c_false) and 1 (= c_true)  allowed
       ms_text_font-doc_class EQ c_docclass_htm.
      rs_table-tab_delimited = c_true.
    ENDIF.

    "With column header is default
    IF rs_table-without_hdr CN '01'.
      rs_table-without_hdr = c_false.
    ENDIF.

    "Only for HTML forms relevant -> No frame as default
    IF ms_text_font-doc_class EQ c_docclass_htm.
      IF rs_table-use_prop_font CN '01'.
        rs_table-use_prop_font = c_true.
      ENDIF.

      IF rs_table-with_frame CN '01'.
        rs_table-with_frame = c_false.
      ENDIF.

      CASE rs_table-with_frame.
        WHEN c_true.
          IF rs_table-frame_width IS INITIAL.
            rs_table-frame_width = c_framewidth_thin.
          ENDIF.
          IF rs_table-frame_style IS INITIAL.
            rs_table-frame_style = c_framestyle_solid.
          ENDIF.
          IF rs_table-frame_color IS INITIAL.
            IF ms_text_font-without_head_body EQ c_true.     "Is e. g. for enhancement in Fiori Inbox
              rs_table-frame_color = c_framewidth_inherit.
            ELSE.
              rs_table-frame_color = 'black' ##no_text.      "Is e. g. for a mail
            ENDIF.
          ENDIF.

        WHEN c_false.
          IF rs_table-frame_width IS INITIAL.
            rs_table-frame_width = c_framewidth_inherit.
          ENDIF.
          IF rs_table-frame_style IS INITIAL.
            rs_table-frame_style = c_framestyle_inherit.
          ENDIF.
          IF rs_table-frame_color IS INITIAL.
            rs_table-frame_color = c_framewidth_inherit.
          ENDIF.
      ENDCASE.

      IF rs_table-v_padding IS INITIAL.
        rs_table-v_padding = '0.25'.     "Upper and lower space between value and border
      ENDIF.

      IF rs_table-h_padding IS INITIAL.
        rs_table-h_padding = '0.5'.      "Left and right space between value and border
      ENDIF.

      IF rs_table-table_desc_alignm IS INITIAL.
        rs_table-table_desc_alignm = c_table_desc_alignm_center.
      ENDIF.
    ENDIF.

    "Distance for RAW preparation
    IF rs_table-col_distance IS INITIAL.
      rs_table-col_distance = 1.
    ENDIF.

    "Optimize columns -> is default
    IF rs_table-col_optimized CN '01'.
      rs_table-col_optimized = c_true.
    ENDIF.
  ENDMETHOD.                    "complete_table_defaults


  METHOD convert_text_2_mail_text.
    "-----------------------------------------------------------------*
    "   Convert (SAP-Script) text into mail text
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      ls_text      TYPE tline,
      ls_mail_text TYPE soli.

    LOOP AT it_text INTO ls_text.
      ls_mail_text-line = ls_text-tdline.
      APPEND ls_mail_text TO rt_mail_text.
    ENDLOOP.
  ENDMETHOD.                    "convert_text_2_mail_text


  METHOD convert_soli_tab_2_html_stream.
    "-----------------------------------------------------------------*
    "   Convert SOLI_TAB into HTML/TEXT stream for Fiori WF descr.
    "-----------------------------------------------------------------*
    result = cl_bcs_convert=>txt_to_string( it_soli = it_details ).
  ENDMETHOD.                    "convert_soli_tab_2_html_table


  METHOD convert_soli_tab_2_html_table.
    "-----------------------------------------------------------------*
    "   Convert SOLI_TAB into TDLINE table for WF description in BO
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      ls_detail_out        TYPE htmlline.

    LOOP AT it_details INTO DATA(ls_detail_in).
      WHILE ls_detail_in-line IS NOT INITIAL.
        ls_detail_out-tdline = ls_detail_in-line.
        APPEND ls_detail_out TO result.

        SHIFT ls_detail_in-line LEFT BY 132 PLACES.
      ENDWHILE.
    ENDLOOP.
  ENDMETHOD.                    "convert_soli_tab_2_html_table


  METHOD create_symbol.
    "-----------------------------------------------------------------*
    "   Create symbols for search (name bracketed in hashes)
    "-----------------------------------------------------------------*
    CLEAR rv_symbol.
    rv_symbol = iv_value_name.
    TRANSLATE rv_symbol TO UPPER CASE.
    CONCATENATE '#' rv_symbol '#' INTO rv_symbol.
  ENDMETHOD.                    "create_symbol


  METHOD create_style_for_table_n_cells.
    "-----------------------------------------------------------------*
    "   Create style addition for the tags 'table', 'th' and 'td'
    "-----------------------------------------------------------------*
    "Result example returned as one string:
    "style="border-collapse: collapse;
    "       border: medium solid #F3C431;
    "       padding: 0.5rem 0.25rem"
    rv_style_addition = |style="border-collapse: collapse; | &
                               |border: { SWITCH #( is_table-frame_width
                                                      WHEN c_framewidth_thin   THEN `thin`
                                                      WHEN c_framewidth_medium THEN `medium`
                                                      WHEN c_framewidth_thick  THEN `thick`
                                                      ELSE `inherit` ) } | &
                               |{ SWITCH #( is_table-with_frame
                                              WHEN c_framestyle_none THEN `none`
                                              WHEN c_framestyle_solid THEN `solid`
                                              WHEN c_framestyle_ridge THEN `ridge`
                                              ELSE `inherit` ) } | &
                               |{ is_table-frame_color }; | &
                               "                 upper and lower          left and right
                               |padding: { is_table-v_padding }rem { is_table-h_padding }rem"| ##no_text.
  ENDMETHOD.                    "create_table_style_addition


  METHOD determine_text_field.
    "-----------------------------------------------------------------*
    "   Determine text field - is it text module or mail length
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lo_stru_desc      TYPE REF TO cl_abap_structdescr,
      lo_elem_desc      TYPE REF TO cl_abap_elemdescr,
      lt_comps          TYPE abap_component_tab,
      ls_comp           TYPE abap_componentdescr,
      lv_full_comp_name TYPE seocpdname.

    FIELD-SYMBOLS:
      <ls_text_line> TYPE data,
      <lv_txt_fld>   TYPE data.

    "Initialize exporting values
    CLEAR: ev_txt_fld_len,
           er_txt_fld.

    "Dereference text line to be able to determine the reference of the requested field
    ASSIGN ir_text_line->* TO <ls_text_line>.
    "Get RTTI description of structured parameter IS_TABLE_LINE
    lo_stru_desc ?= cl_abap_structdescr=>describe_by_data_ref( ir_text_line ).
    "TDLINE = text line of table ET_TEXT       (TLINE)
    "LINE   = text line of table ET_MAIL_TEXT  (SOLI)
    lt_comps = lo_stru_desc->get_components( ).
    LOOP AT lt_comps INTO  ls_comp
                     WHERE name EQ 'TDLINE'
                        OR name EQ 'LINE' ##no_text.
      "Get output length
      lo_elem_desc  ?= ls_comp-type.
      ev_txt_fld_len = lo_elem_desc->output_length.

      "Get reference of text line for assignment in caller method
      CONCATENATE '<LS_TEXT_LINE>-' ##no_text
                  ls_comp-name INTO lv_full_comp_name.
      ASSIGN (lv_full_comp_name) TO <lv_txt_fld>.
      GET REFERENCE OF <lv_txt_fld> INTO er_txt_fld.

      "Only for table replacements check further settings
      IF cs_table IS REQUESTED.
        "Set default values
        CASE ls_comp-name.
          WHEN 'TDLINE' ##no_text.
            "for text module: Only 132 digits length, so only 1 digit distance
            IF cs_table-col_distance IS INITIAL.
              cs_table-col_distance = 1.
            ENDIF.

            "Tabulator has no effect in text modules - set to false in any case
            cs_table-tab_delimited  = c_false.

          WHEN 'LINE' ##no_text.
            "for text module: 255 digits length, the distance can be 2 digits
            IF cs_table-col_distance IS INITIAL.
              IF cs_table-delimiter IS NOT INITIAL.
                cs_table-col_distance = 1.
              ELSE.
                cs_table-col_distance = 2.
              ENDIF.
            ENDIF.

            IF cs_table-tab_delimited IS INITIAL.
              cs_table-tab_delimited = c_false.
            ENDIF.
        ENDCASE.
      ENDIF.

      EXIT.
    ENDLOOP.
  ENDMETHOD.                    "determine_text_field


  METHOD enhance_cell_style_align_right.
    "-----------------------------------------------------------------*
    "   Enhance style for the cell tags 'td' to align the value right
    "-----------------------------------------------------------------*
    "Result example returned as one string:
    "style="border-collapse: collapse;
    "       border: medium solid #F3C431;
    "       border-spacing: 0.5rem 0.25rem;
    "       padding: 0.3rem;
    "       text-align: right"
    rv_style_addition = create_style_for_table_n_cells( is_table ).
    "Replace the last digit (= ") by the alignment addition
    rv_style_addition = replace( val  = rv_style_addition
                                 off  = strlen( rv_style_addition ) - 1
                                 len  = 1
                                 with = `; text-align: right"` ) ##no_text.
  ENDMETHOD.                    "create_style_cell_right_align


  METHOD get_text_module.
    "-----------------------------------------------------------------*
    "   Get text module
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lx_text_mod TYPE REF TO zcx_ca_text_module,
      lx_int      TYPE REF TO zcx_ca_intern,
      ls_text_key TYPE stxh_key.

    "Check if key is complete
    ls_text_key = is_text_key.
    IF ls_text_key-tdobject IS INITIAL.
      ls_text_key-tdobject = 'TEXT' ##no_text.
    ENDIF.

    IF ls_text_key-tdid IS INITIAL.
      ls_text_key-tdid = 'ST' ##no_text.
    ENDIF.

    IF ls_text_key-tdspras IS INITIAL.
      ls_text_key-tdspras = sy-langu.
    ENDIF.

    "Get text module and header
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = ls_text_key-tdid
        language                = ls_text_key-tdspras
        name                    = ls_text_key-tdname
        object                  = ls_text_key-tdobject
      IMPORTING
        header                  = es_thead
      TABLES
        lines                   = et_text
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
      "Everything is fine

    ELSEIF sy-subrc BETWEEN 1 AND 6.
      lx_text_mod ?= zcx_ca_error=>create_exception(
                              iv_excp_cls = zcx_ca_text_module=>c_zcx_ca_text_module
                              iv_function = 'READ_TEXT'
                              iv_subrc    =  sy-subrc ) ##no_text.

      RAISE EXCEPTION lx_text_mod.

    ELSE.
      lx_int ?= zcx_ca_intern=>create_exception(
                                iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                iv_function = 'READ_TEXT'
                                iv_subrc    =  sy-subrc ) ##no_text.
      RAISE EXCEPTION lx_int.
    ENDIF.

*    "When larger texts are converted the text gets smaller -> LT_TXT_CONV has less
*    "entries than ET_TEXT, which is why some of the unconverted text will be transferred
*    LOOP AT lt_txt_conv INTO DATA(ls_txt_conv).
*      MODIFY et_text INDEX sy-tabix
*                     FROM VALUE #( tdline = ls_txt_conv-line )
*                     TRANSPORTING tdline.
*    ENDLOOP.
*
*    DATA(lv_lines_new) = lines( lt_txt_conv ).
*    DATA(lv_lines_old) = lines( et_text ).
*
*    IF lv_lines_old GT lv_lines_new.
*      DELETE et_text FROM lv_lines_new TO lv_lines_old.
*    ENDIF.
  ENDMETHOD.                    "get_text_module


  METHOD get_text_module_n_replace.
    "-----------------------------------------------------------------*
    "   ${description}
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lx_error TYPE REF TO cx_root,
      lt_text  TYPE tline_tab,
      ls_thead TYPE thead.

    "Initialize exporting parameters
    CLEAR: et_text,
           et_mail_text.

    "Get text module
    "C h e c k   i n b o u n d   p a r a m e t e r s
    "Is the text key or the text available?
    IF is_text_key IS INITIAL AND
       it_text     IS INITIAL.
      "At least one of the following parameters must be set: &1 &2 &3 &4
      RAISE EXCEPTION TYPE zcx_ca_text_module
        EXPORTING
          textid   = zcx_ca_text_module=>at_least_one
          mv_msgv1 = 'IS_TEXT_KEY'
          mv_msgv2 = 'IT_TEXT' ##no_text.

    ELSEIF it_text IS INITIAL.
      "If no text is transferred read text module
      get_text_module(
                  EXPORTING
                    is_text_key = is_text_key
                  IMPORTING
                    et_text     = lt_text
                    es_thead    = ls_thead ).

    ELSE.
      lt_text = it_text.
    ENDIF.

    "Set text type and font defaults
    set_text_type_n_font( is_text_font ).

    "Insert a hint when the system is a non-productive system
    insert_hint_for_test(
                    CHANGING
                      ct_text = lt_text ).

    "If no data are available nothing has to be prepared
    IF it_flds_strcs IS INITIAL AND
       it_tables     IS INITIAL AND
       it_links      IS INITIAL.
      "Set available text into exporting fields
      IF et_text IS REQUESTED.
        et_text = lt_text.
      ENDIF.

      IF et_mail_text IS REQUESTED.
        mv_is_mailtext = abap_true.
        et_mail_text = convert_text_2_mail_text( lt_text ).
        "Set line breaks to all lines
        set_html_tag_line_break(
                            CHANGING
                              ct_mail_text = et_mail_text ).
        "Set general font -> proportional font
        set_html_tag_font(
                      CHANGING
                        ct_mail_text = et_mail_text ).
        "Set introduction and declaration
        set_html_tag_intro_n_body(
                              EXPORTING
                                it_tables    = it_tables
                              CHANGING
                                ct_mail_text = et_mail_text ).
      ENDIF.

      "Return to caller
      RETURN.
    ENDIF.

    "Replace standard symbols in text
    IF it_flds_strcs IS NOT INITIAL.
      "Prepare function pool if necessary
      check_n_generate_function_pool( it_flds_strcs ).

      CALL FUNCTION 'Z_CA_REPLACE_SYMBOLS'
        EXPORTING
          is_thead      = ls_thead
          it_flds_strcs = it_flds_strcs
        CHANGING
          ct_text       = lt_text.
    ENDIF.

    TRY.
        "In order to be able to create the list of included out
        "If the text is requested as text module prepare it separately
        IF et_text IS REQUESTED.
          mv_is_mailtext = abap_false.
          "Replace table symbols with prepared table values
          replace_single_symbols(
                            EXPORTING
                              it_flds_strcs = it_flds_strcs
                            CHANGING
                              ct_text       = lt_text ).
          "Replace table symbols with prepared table values
          replace_table_symbols(
                            EXPORTING
                              it_tables = it_tables
                            CHANGING
                              ct_text   = lt_text ).
          "Replace link symbols with prepared link values
          replace_link_symbols(
                          EXPORTING
                            it_links = it_links
                          CHANGING
                            ct_text  = lt_text ).
          et_text = lt_text.
        ENDIF.

        "Because of the width of a mail text the preparation is from
        "here on separated
        "Prepare text as mail text
        IF et_mail_text IS REQUESTED.
          mv_is_mailtext = abap_true.
          et_mail_text = convert_text_2_mail_text( lt_text ).
          "Replace table symbols with prepared table values
          replace_single_symbols(
                            EXPORTING
                              it_flds_strcs = it_flds_strcs
                            CHANGING
                              ct_text       = et_mail_text ).
          "Replace table symbols with prepared table values
          replace_table_symbols(
                          EXPORTING
                            it_tables = it_tables
                          CHANGING
                            ct_text   = et_mail_text ).
          "Replace link symbols with prepared link values
          replace_link_symbols(
                          EXPORTING
                            it_links = it_links
                          CHANGING
                            ct_text  = et_mail_text ).
          "Set line breaks to all lines
          set_html_tag_line_break(
                              CHANGING
                                ct_mail_text = et_mail_text ).
          "Set general font
          set_html_tag_font(
                        CHANGING
                          ct_mail_text = et_mail_text ).
          "Set introduction and declaration
          set_html_tag_intro_n_body(
                                EXPORTING
                                  it_tables    = it_tables
                                CHANGING
                                  ct_mail_text = et_mail_text ).
        ENDIF.

      CATCH zcx_ca_conv
            cx_sy_assign_cast_illegal_cast
            cx_sy_assign_cast_unknown_type
            cx_sy_assign_out_of_range      INTO lx_error.
        "Internal error occurred
        RAISE EXCEPTION TYPE zcx_ca_intern
          EXPORTING
            previous = lx_error.
    ENDTRY.
  ENDMETHOD.                    "get_text_module_n_replace


  METHOD insert_hint_for_test.
    "-----------------------------------------------------------------*
    "   Insert hint that text was created/mailed from a test system
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      ls_text        TYPE tline,
      lv_client_role TYPE cccategory.

    IF ms_text_font-without_hint_test EQ c_true.
      RETURN.
    ENDIF.

    CALL FUNCTION 'TR_SYS_PARAMS'
      IMPORTING
        system_client_role = lv_client_role.

    IF lv_client_role NE 'P' ##no_text.
      "Set paragraph for all lines to avoid deletion while replacing program symbols
      "* * * * *   P A T T E R N   * * * * *       from SAP system'(mst)
      CASE ms_text_font-doc_class.
        WHEN c_docclass_raw.
          ls_text-tdformat = '*'.
          ls_text-tdline   = space.
          INSERT ls_text INTO ct_text INDEX 1.
          ls_text-tdline   = ls_text-tdline = |{ TEXT-mst } { sy-sysid } / { sy-mandt }|.
          INSERT ls_text INTO ct_text INDEX 1.
          ls_text-tdline   = space.
          INSERT ls_text INTO ct_text INDEX 1.

        WHEN c_docclass_htm.
          ls_text-tdformat = '*'.
          ls_text-tdline   = '<p style="text-align:left"></p>' ##no_text.
          INSERT ls_text INTO ct_text INDEX 1.
          ls_text-tdline   = |<p style="text-align:center">{ TEXT-mst } { sy-sysid } / { sy-mandt }</p>|.
          INSERT ls_text INTO ct_text INDEX 1.
          ls_text-tdline   = '<p></p>' ##no_text.
          INSERT ls_text INTO ct_text INDEX 1.
      ENDCASE.
    ENDIF.
  ENDMETHOD.                    "insert_hint_for_test


  METHOD lock_source.
    "-----------------------------------------------------------------*
    "   Lock report souce to append new DDIC objects
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lx_int       TYPE REF TO zcx_ca_intern,
      lv_is_locked TYPE abap_bool.

    lv_is_locked = abap_false.
    DO 5 TIMES.
      "Try to lock include
      CALL FUNCTION 'ENQUEUE_ESRDIRE'
        EXPORTING
          mode_trdir     = 'X'
          name           = c_name_incl_ddic_def
          _scope         = '1'
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.
      CASE sy-subrc.
        WHEN 0.
          lv_is_locked = abap_true.
          EXIT.

        WHEN 1.
          WAIT UP TO 1 SECONDS.

        WHEN OTHERS.
          lx_int ?= zcx_ca_intern=>create_exception(
                                iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                iv_function = 'ENQUEUE_ESRDIRE' ##no_text
                                iv_subrc    = sy-subrc ).
          RAISE EXCEPTION lx_int.
      ENDCASE.
    ENDDO.

    IF lv_is_locked EQ abap_false.
      sy-msgv1 = c_name_incl_ddic_def.
      "Program &1 could not be locked - please try again later
      RAISE EXCEPTION TYPE zcx_ca_text_module
        EXPORTING
          textid   = zcx_ca_text_module=>lock_not_possible
          mv_msgv1 = sy-msgv1.
    ENDIF.
  ENDMETHOD.                    "lock_source


  METHOD output_single_column.
    "-----------------------------------------------------------------*
    "   Output of a single column
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
*     Object references of RTTS for dynamic data type determination
      lo_elem_desc     TYPE REF TO cl_abap_elemdescr,
      lr_field_cpy     TYPE REF TO data,
      lr_fld_output    TYPE REF TO data,
      lv_use_curr      TYPE abap_bool     VALUE abap_false,
      lv_use_unit      TYPE abap_bool     VALUE abap_false,
      lv_output_length TYPE i,
      lv_len           TYPE i,
      lv_link          TYPE zca_d_link,
      lv_link_name     TYPE zca_d_name_link.

    FIELD-SYMBOLS:
      <lv_field>      TYPE data,           "Value of single column
      <lv_field_cpy>  TYPE data,           "local copy of column value
      <lv_currency>   TYPE data,
      <lv_unit>       TYPE data,
      <lv_link_name>  TYPE data,
      <lv_fld_output> TYPE c.              "Converted value

    "Set pointer on column
    ASSIGN COMPONENT is_stru_comp-s_comp-name OF STRUCTURE is_data_line TO <lv_field>.
    IF sy-subrc NE 0.
      ASSIGN is_data_line TO <lv_field>.
      IF <lv_field> IS NOT ASSIGNED.
        RETURN.
      ENDIF.
    ENDIF.

    "Downcast data description into element description
    lo_elem_desc ?= is_stru_comp-s_comp-type.

    "Create a local copy of the current column (to be able to change the value)
    "and copy the value to it
    CREATE DATA lr_field_cpy TYPE HANDLE lo_elem_desc.
    ASSIGN lr_field_cpy->* TO <lv_field_cpy>.
    <lv_field_cpy> = <lv_field>.

    "Try to set currency or unit for corresponding fields
    IF is_stru_comp-s_dfies    IS NOT INITIAL                    AND
       lo_elem_desc->type_kind EQ lo_elem_desc->typekind_packed.
      "If a reference field is defined try to address this field
      "for correct conversion
      IF is_stru_comp-s_dfies-reffield IS NOT INITIAL.
        CASE is_stru_comp-s_dfies-datatype.
          WHEN 'CURR' ##no_text.
            ASSIGN COMPONENT is_stru_comp-s_dfies-reffield
                OF STRUCTURE is_data_line TO <lv_currency>.
            IF sy-subrc EQ 0.
              lv_use_curr = abap_true.
            ENDIF.

          WHEN 'QUAN' ##no_text.
            ASSIGN COMPONENT is_stru_comp-s_dfies-reffield
                OF STRUCTURE is_data_line TO <lv_unit>.
            IF sy-subrc EQ 0.
              lv_use_unit = abap_true.
            ENDIF.
        ENDCASE.
      ENDIF.
    ENDIF.

    "Create a character field for output and make it longer for numeric values
    IF lo_elem_desc->type_kind EQ lo_elem_desc->typekind_packed OR
       lo_elem_desc->type_kind EQ lo_elem_desc->typekind_int    OR
       lo_elem_desc->type_kind EQ lo_elem_desc->typekind_int1   OR
       lo_elem_desc->type_kind EQ lo_elem_desc->typekind_int2.
      lv_output_length = lo_elem_desc->output_length + 2.

    ELSEIF is_output_fld-as_link EQ c_true.
      "Column should be prepared as link -> use length after preparation
      "Set pointer on link name
      ASSIGN COMPONENT is_output_fld-fld_link_name
                                      OF STRUCTURE is_data_line TO <lv_link_name>.
      IF sy-subrc EQ 0.
        lv_link_name = <lv_link_name>.
      ENDIF.
      lv_link = <lv_field_cpy>.
      "Prepare link for HTML output - in Outlook 2003 a link is not
      "recognized as link if it is not declared as link
      lv_link = set_html_tag_for_link( iv_name = lv_link_name
                                       iv_link = lv_link ).
      <lv_field_cpy> = lv_link.
      lv_output_length = strlen( <lv_field_cpy> ).

    ELSE.
      lv_output_length = lo_elem_desc->output_length.
    ENDIF.

    "Create data object for output
    CREATE DATA lr_fld_output TYPE c LENGTH lv_output_length.
    ASSIGN lr_fld_output->* TO <lv_fld_output>.

    "Convert SAP internal value into output format
    IF lv_use_curr EQ abap_true.
      zcl_ca_conv=>internal_2_external(
                                  EXPORTING
                                    internal_value = <lv_field_cpy>
                                    currency       = <lv_currency>
                                  IMPORTING
                                    external_value = <lv_fld_output> ).

    ELSEIF lv_use_unit EQ abap_true.
      zcl_ca_conv=>internal_2_external(
                                  EXPORTING
                                    internal_value  = <lv_field_cpy>
                                    unit_of_measure = <lv_unit>
                                  IMPORTING
                                    external_value  = <lv_fld_output> ).

    ELSE.
      IF ( lo_elem_desc->type_kind EQ lo_elem_desc->typekind_date    OR
           lo_elem_desc->type_kind EQ lo_elem_desc->typekind_time ) AND
           <lv_field_cpy>          IS INITIAL.
        CLEAR <lv_fld_output>.

      ELSE.
        zcl_ca_conv=>internal_2_external(
                                    EXPORTING
                                      internal_value = <lv_field_cpy>
                                    IMPORTING
                                      external_value = <lv_fld_output> ).

        IF lo_elem_desc->type_kind EQ lo_elem_desc->typekind_time.
          "Cut off seconds at time values
          <lv_fld_output>  = <lv_fld_output>(5).
          lv_output_length = 5.
        ENDIF.
      ENDIF.
    ENDIF.

    "Justify numeric values
    IF lo_elem_desc->type_kind EQ lo_elem_desc->typekind_packed OR
       lo_elem_desc->type_kind EQ lo_elem_desc->typekind_int    OR
       lo_elem_desc->type_kind EQ lo_elem_desc->typekind_int1   OR
       lo_elem_desc->type_kind EQ lo_elem_desc->typekind_int2.
      CASE ms_text_font-doc_class.
        WHEN c_docclass_raw.
          "S A P   o f f i c e   t e x t
          "Justify numeric values right, since the conversion method returned it
          "left-justified in any case.
          IF lo_elem_desc->is_ddic_type( ) EQ abap_false OR   "then it has always a sign
             is_stru_comp-s_dfies-sign     EQ abap_true.
            lv_len = lv_output_length - 1.
            SHIFT <lv_fld_output>(lv_len) RIGHT DELETING TRAILING space.
          ELSE.
            SHIFT <lv_fld_output> RIGHT DELETING TRAILING space.
          ENDIF.

        WHEN c_docclass_htm.
          "H T M L   t e x t
          "Concatenate a sign to be able to include an align-statement later on
          IF ( lo_elem_desc->is_ddic_type( ) EQ abap_false    OR   "then it has always a sign
               is_stru_comp-s_dfies-sign     EQ abap_true  ) AND
               <lv_fld_output>               NA '-'.
            CONCATENATE '$$'
                        <lv_fld_output>
                        '+'             INTO <lv_fld_output>.
          ELSE.
            CONCATENATE '$$'
                        <lv_fld_output> INTO <lv_fld_output>.
          ENDIF.
      ENDCASE.
    ENDIF.

    "Attach value to output line, incl. tabulator / column tags
    attach_column_to_output_line(
                             EXPORTING
                               iv_fld_output  = <lv_fld_output>
                               iv_fld_len     = lv_output_length
                               is_table       = is_table
                               iv_txt_fld_len = iv_txt_fld_len
                             CHANGING
                               cv_txt_fld     = cv_txt_fld
                               cv_pos         = cv_pos ).
  ENDMETHOD.                    "output_single_column


  METHOD create_table_header.
    "-----------------------------------------------------------------*
    "   Create table header
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      ls_stru_comp         TYPE ty_s_stru_comp.

    IF is_table-t_output_flds IS INITIAL  OR
       is_table-sign_for_flds EQ c_sign_e.
      LOOP AT it_stru_comps INTO  ls_stru_comp
                            WHERE s_comp-type->kind EQ cl_abap_typedescr=>kind_elem
                              AND s_dfies-fieldname IN mra_output_flds.
        "Output of column header
        output_single_column_header(
                               EXPORTING
                                 is_table       = is_table
                                 is_stru_comp   = ls_stru_comp
                                 iv_txt_fld_len = iv_txt_fld_len
                               CHANGING
                                 cv_txt_fld     = cv_txt_fld
                                 cv_pos         = cv_pos ).

        "Next column can not be printed - leave loop
        IF cv_pos EQ c_length_too_long.
          EXIT.
        ENDIF.
      ENDLOOP.

    ELSE.
      "Use only the transmitted output fields in order as requested
      LOOP AT is_table-t_output_flds INTO DATA(ls_output_fld).
        "Get description of component
        READ TABLE it_stru_comps INTO ls_stru_comp
                   WITH TABLE KEY s_comp-name = ls_output_fld-fieldname.
        IF sy-subrc                       NE 0                             OR
           ls_stru_comp-s_comp-type->kind NE cl_abap_typedescr=>kind_elem.
          CONTINUE.
        ENDIF.

        "Output of column header
        output_single_column_header(
                                   EXPORTING
                                     is_table       = is_table
                                     is_stru_comp   = ls_stru_comp
                                     iv_txt_fld_len = iv_txt_fld_len
                                   CHANGING
                                     cv_txt_fld     = cv_txt_fld
                                     cv_pos         = cv_pos ).

        "Next column can not be printed - leave loop
        IF cv_pos EQ c_length_too_long.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.                    "create_table_header


  METHOD create_table_line.
    "-----------------------------------------------------------------*
    "   Create a table line
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      ls_stru_comp         TYPE ty_s_stru_comp.

    "Select only elementary types -> no references or complex types
    IF is_table-t_output_flds IS INITIAL  OR
       is_table-sign_for_flds EQ c_sign_e.
      LOOP AT it_stru_comps INTO  ls_stru_comp
                            WHERE s_comp-type->kind EQ cl_abap_typedescr=>kind_elem
                              AND s_dfies-fieldname IN mra_output_flds.
        "Output of column header
        output_single_column(
                        EXPORTING
                          is_table       = is_table
                          is_stru_comp   = ls_stru_comp
                          iv_txt_fld_len = iv_txt_fld_len
                          is_data_line   = is_data_line
                        CHANGING
                          cv_txt_fld     = cv_txt_fld
                          cv_pos         = cv_pos ).
        "Next column can not be printed - leave loop
        IF cv_pos EQ c_length_too_long.
          EXIT.
        ENDIF.
      ENDLOOP.

    ELSE.
      "Use only the transmitted output fields in the order as it is
      LOOP AT is_table-t_output_flds INTO DATA(ls_output_fld).
        "Get description of component
        READ TABLE it_stru_comps INTO ls_stru_comp
                   WITH TABLE KEY s_comp-name = ls_output_fld-fieldname.
        IF sy-subrc                       NE 0                             OR
           ls_stru_comp-s_comp-type->kind NE cl_abap_typedescr=>kind_elem.
          CONTINUE.
        ENDIF.

        "Output of column header
        output_single_column(
                        EXPORTING
                          is_table       = is_table
                          is_output_fld  = ls_output_fld
                          is_stru_comp   = ls_stru_comp
                          iv_txt_fld_len = iv_txt_fld_len
                          is_data_line   = is_data_line
                        CHANGING
                          cv_txt_fld     = cv_txt_fld
                          cv_pos         = cv_pos ).
        "Next column can not be printed - leave loop
        IF cv_pos EQ c_length_too_long.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.                    "create_table_line


  METHOD output_single_column_header.
    "-----------------------------------------------------------------*
    "   Output of a single column header
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
*     Object references of RTTS for dynamic data type determination
      lo_elem_desc     TYPE REF TO cl_abap_elemdescr,
*     other fields
      lr_fld_output    TYPE REF TO data,
      ls_dfies         TYPE dfies,          "DDIC description of structure component
      lv_col_hdr_txt   TYPE reptext,
      lv_output_length TYPE syfdpos,        "Output length
      lv_len           TYPE syfdpos.        "Output length of value

    FIELD-SYMBOLS:
      <lv_fld_output>      TYPE c.              "Converted value

    "Downcast data description into element description
    lo_elem_desc ?= is_stru_comp-s_comp-type.

    CASE ms_text_font-doc_class.
      WHEN c_docclass_htm.
        lv_output_length = lo_elem_desc->output_length.
        DATA(lv_ltx) = strlen( is_stru_comp-s_dfies-scrtext_l ).
        DATA(lv_mtx) = strlen( is_stru_comp-s_dfies-scrtext_m ).
        DATA(lv_stx) = strlen( is_stru_comp-s_dfies-scrtext_s ).
        DATA(lv_rtx) = strlen( is_stru_comp-s_dfies-reptext ).

        ASSIGN lv_col_hdr_txt TO <lv_fld_output>.
        <lv_fld_output> = is_stru_comp-s_dfies-reptext.
        IF lv_stx LE lv_output_length AND
           lv_rtx LE lv_stx.
          <lv_fld_output> = is_stru_comp-s_dfies-scrtext_s.
        ENDIF.

        IF lv_mtx LE lv_output_length AND
           lv_rtx LE lv_mtx.
          <lv_fld_output> = is_stru_comp-s_dfies-scrtext_m.
        ENDIF.

        IF lv_ltx LE lv_output_length AND
           lv_rtx LE lv_ltx.
          <lv_fld_output> = is_stru_comp-s_dfies-scrtext_l.
        ENDIF.

        "If no key word is defined use component name
        IF <lv_fld_output> IS INITIAL.
          ls_dfies-reptext = is_stru_comp-s_comp-name.
          TRANSLATE ls_dfies-reptext+1 TO LOWER CASE.
          <lv_fld_output> = ls_dfies-reptext.
        ENDIF.

        lv_output_length = strlen( <lv_fld_output> ).

      WHEN c_docclass_raw.
        "Column header for numeric values, like amounts or quantities, will be
        "justified right and respecting the sign. So the need another length
        IF lo_elem_desc->type_kind EQ lo_elem_desc->typekind_packed OR
           lo_elem_desc->type_kind EQ lo_elem_desc->typekind_int    OR
           lo_elem_desc->type_kind EQ lo_elem_desc->typekind_int1   OR
           lo_elem_desc->type_kind EQ lo_elem_desc->typekind_int2.
          lv_output_length = lo_elem_desc->output_length + 2.

        ELSE.
          lv_output_length = lo_elem_desc->output_length.
        ENDIF.

        "Create a character field for output
        CREATE DATA lr_fld_output TYPE c LENGTH lv_output_length.
        ASSIGN lr_fld_output->* TO <lv_fld_output>.

        "Is column defined in DDIC?
        CASE lo_elem_desc->is_ddic_type( ).
          WHEN abap_false.
            "For local defined fields use component name as column header
            ls_dfies-reptext = is_stru_comp-s_comp-name.
            TRANSLATE ls_dfies-reptext+1 TO LOWER CASE.
            <lv_fld_output> = ls_dfies-reptext.

          WHEN abap_true.
            IF is_stru_comp-s_dfies-reptext IS NOT INITIAL.
              <lv_fld_output> = is_stru_comp-s_dfies-reptext.

            ELSEIF is_stru_comp-s_dfies-scrtext_s IS NOT INITIAL.
              <lv_fld_output> = is_stru_comp-s_dfies-scrtext_s.

            ELSEIF is_stru_comp-s_dfies-scrtext_m IS NOT INITIAL.
              <lv_fld_output> = is_stru_comp-s_dfies-scrtext_m.

            ELSEIF is_stru_comp-s_dfies-scrtext_l IS NOT INITIAL.
              <lv_fld_output> = is_stru_comp-s_dfies-scrtext_l.

            ELSE.
              "If no key word is defined use component name
              ls_dfies-reptext = is_stru_comp-s_comp-name.
              TRANSLATE ls_dfies-reptext+1 TO LOWER CASE.
              <lv_fld_output> = ls_dfies-reptext.
            ENDIF.
        ENDCASE.

        "Column header for numeric values, like amounts or quantities, will be
        "justified right and respecting the sign. So they need another length
        IF lo_elem_desc->type_kind EQ lo_elem_desc->typekind_packed OR
           lo_elem_desc->type_kind EQ lo_elem_desc->typekind_int    OR
           lo_elem_desc->type_kind EQ lo_elem_desc->typekind_int1   OR
           lo_elem_desc->type_kind EQ lo_elem_desc->typekind_int2.
          IF lo_elem_desc->is_ddic_type( ) EQ abap_false OR   "then it has always a sign
             is_stru_comp-s_dfies-sign     EQ abap_true.
            lv_len = lv_output_length - 1.
            SHIFT <lv_fld_output>(lv_len) RIGHT DELETING TRAILING space.
          ELSE.
            SHIFT <lv_fld_output> RIGHT DELETING TRAILING space.
          ENDIF.

        ELSEIF lo_elem_desc->type_kind EQ lo_elem_desc->typekind_float.
          SHIFT <lv_fld_output> RIGHT DELETING TRAILING space.
        ENDIF.
    ENDCASE.

    "Attach value to output line, incl. tabulator / column tags
    attach_column_to_output_line(
                           EXPORTING
                             iv_fld_output  = <lv_fld_output>
                             iv_fld_len     = lv_output_length
                             is_table       = is_table
                             iv_txt_fld_len = iv_txt_fld_len
                             iv_is_header   = abap_false
                           CHANGING
                             cv_txt_fld     = cv_txt_fld
                             cv_pos         = cv_pos ).
  ENDMETHOD.                    "output_single_column_header


  METHOD prepare_link_lines.
    "-----------------------------------------------------------------*
    "   Prepare link lines - Description what to do and the link
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lr_link_line   TYPE REF TO data,    "Workarea text lines
      lr_txt_fld     TYPE REF TO data,    "Field for text in WA LR_LINK_LINE
      lt_link_text   TYPE tline_tab,      "Text module for link explanation
      ls_thead       TYPE thead ##needed,
      lv_txt_fld_len TYPE syfdpos,        "Length of text line
      lv_link        TYPE zca_d_link,
      lv_link_len    TYPE syfdpos,        "Length of link
      lx_error       TYPE REF TO cx_root.

    FIELD-SYMBOLS:
      <ls_link_line> TYPE data,
      <lv_txt_fld>   TYPE data.

    "Initialize link text lines
    CLEAR et_link_lines.

    "Leave, if no link and no description available
    IF is_link-link      IS INITIAL AND
       is_link-link_desc IS INITIAL.
      RETURN.
    ENDIF.

    "T e c h n i c a l   p r e p a r a t i o n s
    TRY.
        "Create a workarea for CHANGING parameter ET_LINK_LINES
        CREATE DATA lr_link_line LIKE LINE OF et_link_lines.
        ASSIGN lr_link_line->* TO <ls_link_line>.

      CATCH cx_sy_create_data_error INTO lx_error.
        "Error occurred while creating a data object
        RAISE EXCEPTION TYPE zcx_ca_intern
          EXPORTING
            textid   = zcx_ca_intern=>data_creation_failed
            previous = lx_error.
    ENDTRY.

    "Determine text field and its length for output
    determine_text_field(
                     EXPORTING
                       ir_text_line   = lr_link_line
                     IMPORTING
                       ev_txt_fld_len = lv_txt_fld_len
                       er_txt_fld     = lr_txt_fld ).
    ASSIGN lr_txt_fld->* TO <lv_txt_fld>.

    "Prepare link for HTML output - in Outlook 2003 the link is not
    "recognized as link if it is declared as link
    lv_link = set_html_tag_for_link( iv_name = is_link-link_name
                                     iv_link = is_link-link ).

    "Set explanation text for link
    IF is_link-s_text_key IS NOT INITIAL.
      "Get text module as link explanation
      get_text_module(
                  EXPORTING
                    is_text_key = is_link-s_text_key
                  IMPORTING
                    et_text     = lt_link_text
                    es_thead    = ls_thead ).

      CASE lv_txt_fld_len.
        WHEN c_length_text.             "= Text module (TLINE)
          APPEND LINES OF lt_link_text TO et_link_lines.

        WHEN c_length_mail.             "= Mail text (SOLI)
          et_link_lines = convert_text_2_mail_text( lt_link_text ).
      ENDCASE.

    ELSEIF is_link-link_desc IS NOT INITIAL.
      "Is text line long enough for link?
      lv_link_len = strlen( is_link-link_desc ).
      IF lv_link_len GT lv_txt_fld_len.
        sy-msgv2 = is_link-name.
        WRITE: lv_link_len    NO-ZERO LEFT-JUSTIFIED TO sy-msgv3,
               lv_txt_fld_len NO-ZERO LEFT-JUSTIFIED TO sy-msgv4.
        "Beschreibung für &1 &2 ist zu lang (&3) für die Textzeile (&4)
        RAISE EXCEPTION TYPE zcx_ca_text_module
          EXPORTING
            textid   = zcx_ca_text_module=>descr_too_long
            mv_msgv1 = 'Link'(lnk)
            mv_msgv2 = sy-msgv2
            mv_msgv3 = sy-msgv3
            mv_msgv4 = sy-msgv4.
      ENDIF.

      <lv_txt_fld> = is_link-link_desc.
      "Field <lv_txt_fld> is part of <ls_link_line> - no further actions necessary
      APPEND <ls_link_line> TO et_link_lines.
    ENDIF.

    "Set link
    IF lv_link IS NOT INITIAL.
      APPEND LINES OF cl_bcs_convert=>string_to_soli( CONV #( lv_link ) ) TO et_link_lines.
    ENDIF.
  ENDMETHOD.                    "prepare_link_lines


  METHOD prepare_table_lines.
    "-----------------------------------------------------------------*
    "   Prepare table data (= output)
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lx_error          TYPE REF TO cx_root,
*     Object references of RTTS for dynamic data type determination
      lo_tabl_desc      TYPE REF TO cl_abap_tabledescr,
      lo_data_desc      TYPE REF TO cl_abap_datadescr,
      lo_stru_desc      TYPE REF TO cl_abap_structdescr,
      lo_elem_desc      TYPE REF TO cl_abap_elemdescr,
*     Data references for workareas
      lr_text_line      TYPE REF TO data,    "Workarea text lines
      lr_txt_fld        TYPE REF TO data,    "Field for text in WA LR_TEXT_LINE
      lr_data_line      TYPE REF TO data,    "Workarea data lines
*     Table/WA: Description of structure components
      lt_str_incl_comps TYPE abap_component_view_tab,
      lt_stru_comps     TYPE ty_t_stru_comps,
      ls_stru_comp      TYPE ty_s_stru_comp,
      lt_dfies          TYPE ddfields,
*     other fields
      ls_output_fld     TYPE zca_s_output_field,
      lv_pos            TYPE syfdpos,        "Position of output
      lv_txt_fld_len    TYPE syfdpos,        "Length of text line
      lv_descr_len      TYPE i.

    FIELD-SYMBOLS:
      <lt_data_lines> TYPE ANY TABLE,
      <ls_data_line>  TYPE data,
      <ls_text_line>  TYPE data,
      <lv_txt_fld>    TYPE data.

    "Initialize table text lines
    CLEAR et_text.

    "Leave, if no table data are available
    IF cs_table-value IS NOT BOUND.
      RETURN.
    ENDIF.

    "T e c h n i c a l   p r e p a r a t i o n s
    "Is the data reference a table?
    lo_tabl_desc ?= cl_abap_tabledescr=>describe_by_data_ref( cs_table-value ).
    IF lo_tabl_desc->kind NE lo_tabl_desc->kind_table.
      sy-msgv1 = cs_table-name.
      "Value to table parameter &1 is not a table
      RAISE EXCEPTION TYPE zcx_ca_text_module
        EXPORTING
          textid   = zcx_ca_text_module=>not_a_table
          mv_msgv1 = sy-msgv1.
    ENDIF.

    "Get structure description
    lo_data_desc = lo_tabl_desc->get_table_line_type( ).

    "Dereference table object and check if it has data
    ASSIGN cs_table-value->* TO <lt_data_lines>.
    IF <lt_data_lines> IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        "Create a workarea for CHANGING parameter ET_TEXT
        CREATE DATA lr_text_line LIKE LINE OF et_text.
        ASSIGN lr_text_line->* TO <ls_text_line>.

        "Create a workarea for the inbound data table on base of
        "the RTTI line description
        CREATE DATA lr_data_line TYPE HANDLE lo_data_desc.
        ASSIGN lr_data_line->* TO <ls_data_line>.

      CATCH cx_sy_create_data_error INTO lx_error.
        "Error occurred while creating a data object
        RAISE EXCEPTION TYPE zcx_ca_intern
          EXPORTING
            textid   = zcx_ca_intern=>data_creation_failed
            previous = lx_error.
    ENDTRY.

    "Determine text field and its length for output
    determine_text_field(
                     EXPORTING
                       ir_text_line   = lr_text_line
                     IMPORTING
                       ev_txt_fld_len = lv_txt_fld_len
                       er_txt_fld     = lr_txt_fld
                     CHANGING
                       cs_table       = cs_table ).
    ASSIGN lr_txt_fld->* TO <lv_txt_fld>.

    CASE lo_data_desc->kind.
      WHEN lo_data_desc->kind_elem.
        "Downcast into structure description
        CLEAR ls_stru_comp.
        lo_elem_desc ?= lo_data_desc.
        ls_stru_comp-s_comp-name = lo_elem_desc->get_relative_name( ).
        ls_stru_comp-s_comp-type = lo_elem_desc.
        IF lo_elem_desc->is_ddic_type( ) EQ abap_true.
          ls_stru_comp-s_dfies = lo_elem_desc->get_ddic_field( ).
        ENDIF.
        APPEND ls_stru_comp TO lt_stru_comps.

      WHEN lo_data_desc->kind_struct.
        "Downcast into structure description
        lo_stru_desc ?= lo_data_desc.
        "Get components inclusive resolved includes as deep as possible
        lt_str_incl_comps = lo_stru_desc->get_included_view( 9 ).

        "Get components of the structure - LO_STRU_DESC supplies no CURR/QUAN reference field
        CASE lo_data_desc->is_ddic_type( ).
          WHEN abap_true.
            "Enhance with DDIC description of structure
            lt_dfies = lo_stru_desc->get_ddic_field_list( ).
            LOOP AT lt_str_incl_comps INTO ls_stru_comp-s_comp.
              CLEAR ls_stru_comp-s_dfies.
              READ TABLE lt_dfies INTO ls_stru_comp-s_dfies "#EC WARNOK
                                  WITH KEY fieldname = ls_stru_comp-s_comp-name.
              IF sy-subrc EQ 0.
                APPEND ls_stru_comp TO lt_stru_comps.
              ENDIF.
            ENDLOOP.

          WHEN abap_false.
            "Structure is  N O T  in DDIC defined
            LOOP AT lt_str_incl_comps INTO ls_stru_comp-s_comp.
              "Cast type to elementary description
              IF ls_stru_comp-s_comp-type->kind EQ cl_abap_datadescr=>kind_elem.
                lo_elem_desc ?= ls_stru_comp-s_comp-type.
                CLEAR ls_stru_comp-s_dfies.

                IF lo_elem_desc->is_ddic_type( ) EQ abap_true.
                  "Get DDIC description if typed by DDIC data element
                  ls_stru_comp-s_dfies = lo_elem_desc->get_ddic_field( ).
                ENDIF.

                APPEND ls_stru_comp TO lt_stru_comps.
              ENDIF.
            ENDLOOP.
        ENDCASE.
    ENDCASE.

    "Sort requested columns
    IF cs_table-t_output_flds IS NOT INITIAL.
      IF cs_table-sign_for_flds EQ c_sign_e.
        mra_output_flds = VALUE #( FOR ls_ofld IN cs_table-t_output_flds
                                                     ( sign   = cs_table-sign_for_flds
                                                       option = c_opt_eq
                                                       low    = ls_ofld-fieldname ) ).
      ENDIF.

      LOOP AT cs_table-t_output_flds TRANSPORTING NO FIELDS
                                     WHERE col_order IS NOT INITIAL.
        EXIT.
      ENDLOOP.
      "Found a column order? Then sort it. Otherwise keep it as it is!
      IF sy-subrc EQ 0.
        SORT cs_table-t_output_flds BY col_order fieldname.
      ENDIF.
    ENDIF.

    "T a b l e   h e a d e r
    IF cs_table-table_desc IS NOT INITIAL.
      CASE ms_text_font-doc_class.
        WHEN c_docclass_raw.
          <lv_txt_fld> = cs_table-table_desc.

        WHEN c_docclass_htm.
          "Caption must be first statement after open <TABLE> tag
          <lv_txt_fld> = |<caption style="text-align: {
                                      SWITCH #( cs_table-table_desc_alignm
                                                  WHEN c_table_desc_alignm_center THEN `center`
                                                  WHEN c_table_desc_alignm_left   THEN `left`
                                                  WHEN c_table_desc_alignm_right  THEN `right`
                                                  ELSE `inherit` ) }">{ cs_table-table_desc }</caption>| ##no_text.
      ENDCASE.

      "Field <lv_txt_fld> is part of <ls_text_line> - no further actions necessary
      APPEND <ls_text_line> TO et_text.
    ENDIF.

    "C r e a t e   c o l u m n   h e a d e r
    "Create column header if requested
    IF cs_table-without_hdr EQ c_false.
      add_table_tag cs_htmltag-open_tab_hdr.     "<thead>
      add_table_tag cs_htmltag-open_tab_row.     "<tr>

      create_table_header(
                     EXPORTING
                       it_stru_comps  = lt_stru_comps
                       is_table       = cs_table
                       iv_txt_fld_len = lv_txt_fld_len
                     CHANGING
                       cv_txt_fld     = <lv_txt_fld>
                       cv_pos         = lv_pos ).

      "Field <lv_txt_fld> is part of <ls_text_line> - no further actions necessary
      APPEND <ls_text_line> TO et_text.

      add_table_tag cs_htmltag-close_tab_row.    "</tr>
      add_table_tag cs_htmltag-close_tab_hdr.    "</thead>
    ENDIF.

    "O u t p u t   o f   r e q u e s t e d   c o l u m n s   for each data line
    add_table_tag cs_htmltag-open_tab_body.      "<tbody>

    LOOP AT <lt_data_lines> INTO <ls_data_line>.
      add_table_tag cs_htmltag-open_tab_row.     "<tr>

      "Initialize fields for next line
      CLEAR: <ls_text_line>,
             lv_pos.

      create_table_line(
                    EXPORTING
                      is_data_line   = <ls_data_line>
                      it_stru_comps  = lt_stru_comps
                      is_table       = cs_table
                      iv_txt_fld_len = lv_txt_fld_len
                    CHANGING
                      cv_txt_fld     = <lv_txt_fld>
                      cv_pos         = lv_pos ).

      "Field <lv_txt_fld> is part of <ls_text_line> - no further actions necessary
      APPEND <ls_text_line> TO et_text.
      add_table_tag cs_htmltag-close_tab_row.    "</tr>
    ENDLOOP.

    add_table_tag cs_htmltag-close_tab_body.     "</tbody>

    "Enclose prepared table with tags
    set_html_tag_table(
                  EXPORTING
                    is_table     = cs_table
                  CHANGING
                    ct_mail_text = et_text ).
    "Change font to monospaced
    set_html_tag_font(
                  EXPORTING
                    iv_use_prop_font = cs_table-use_prop_font
                  CHANGING
                    ct_mail_text     = et_text ).

    "Is there a description / introduction for this table. Insertion after
    "preparation of table lines to avoid that this line get any table tags.
    IF et_text             IS NOT INITIAL AND
       cs_table-table_desc IS NOT INITIAL.
      IF lv_txt_fld_len EQ c_length_text.
        "Is text line long enough for link?
        lv_descr_len = strlen( cs_table-table_desc ).
        IF lv_descr_len GT lv_txt_fld_len.
          sy-msgv2 = cs_table-name.
          WRITE: lv_descr_len   NO-ZERO LEFT-JUSTIFIED TO sy-msgv3,
                 lv_txt_fld_len NO-ZERO LEFT-JUSTIFIED TO sy-msgv4.
          "Beschreibung für &1 &2 ist zu lang (&3) für die Textzeile (&4)
          RAISE EXCEPTION TYPE zcx_ca_text_module
            EXPORTING
              textid   = zcx_ca_text_module=>descr_too_long
              mv_msgv1 = 'Table'(tbl)
              mv_msgv2 = sy-msgv2
              mv_msgv3 = sy-msgv3
              mv_msgv4 = sy-msgv4.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "prepare_table_lines


  METHOD provide_cell_with_style.
    "-----------------------------------------------------------------*
    "   Provide style addition for the cell tags td and th
    "-----------------------------------------------------------------*
    rv_mail_line = iv_mail_line.
    "The blank at the beginning of the string is the important delimiter between tag and style!!
    DATA(lv_style_table_n_cells) = | { iv_style_table_n_cells }>|.

    IF rv_mail_line(4) EQ cs_htmltag-open_tab_hdr_col   OR
       rv_mail_line(4) EQ cs_htmltag-open_tab_data_col.
      "Replace the last tag digit (= >) by the alignment addition
      rv_mail_line = replace( val  = rv_mail_line
                              off  = 3
                              len  = 1
                              with = lv_style_table_n_cells ).
    ENDIF.
  ENDMETHOD.                    "provide_cell_with_style


  METHOD replace_link_symbols.
    "-----------------------------------------------------------------*
    "   Replace link symbols
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lr_link_lines   TYPE REF TO data,
      ls_link         TYPE zca_s_text_n_link,
      lv_symbol       TYPE string,
      ls_match_result TYPE match_result,
      lx_error        TYPE REF TO cx_root.

    FIELD-SYMBOLS:
      <lt_link_lines>      TYPE STANDARD TABLE.

    "Leave, if no links available
    IF it_links IS INITIAL.
      RETURN.
    ENDIF.

    "Since the CHANGING parameter CT_TEXT can have different types
    "create a table object dynamically to collect the link lines.
    TRY.
        CREATE DATA lr_link_lines LIKE ct_text.
        ASSIGN lr_link_lines->* TO <lt_link_lines>.

      CATCH cx_sy_create_data_error INTO lx_error.
        "Error occurred while creating a data object
        RAISE EXCEPTION TYPE zcx_ca_intern
          EXPORTING
            textid   = zcx_ca_intern=>data_creation_failed
            previous = lx_error.
    ENDTRY.

    "Prepare link lines for each entry
    LOOP AT it_links INTO ls_link.
      "Search for link symbol = #link_name#
      CLEAR ls_match_result.
      lv_symbol = create_symbol( ls_link-name ).
      FIND FIRST OCCURRENCE OF lv_symbol
                      IN TABLE ct_text IN CHARACTER MODE
                       RESULTS ls_match_result.
      IF sy-subrc NE 0.
        "Try next entry if symbol wasn't found
        CONTINUE.
      ENDIF.

      "Prepare link lines
      prepare_link_lines(
                     EXPORTING
                       is_link       = ls_link
                     IMPORTING
                       et_link_lines = <lt_link_lines> ).

      "Delete line with symbol to insert table lines instead
      DELETE ct_text INDEX ls_match_result-line.

      "Insert prepared tables lines into text
      INSERT LINES OF <lt_link_lines> INTO  ct_text
                                      INDEX ls_match_result-line.
    ENDLOOP.
  ENDMETHOD.                    "replace_link_symbols


  METHOD replace_single_symbols.
    "-----------------------------------------------------------------*
    "   Replace single symbols /fields
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lo_type_desc  TYPE REF TO cl_abap_typedescr,
      is_flds_strcs TYPE zca_s_param,
      lv_value      TYPE so_text255,
      lv_symbol     TYPE string.

    FIELD-SYMBOLS:
      <lv_value>           TYPE data.

    "Move inbound values into global structures for symbol replacement
    LOOP AT it_flds_strcs INTO is_flds_strcs.
      "Get name of inbound value if it is not set
      lo_type_desc = cl_abap_typedescr=>describe_by_data_ref( is_flds_strcs-value ).
      "Skip values that are not of flat structured type
      IF lo_type_desc->kind NE lo_type_desc->kind_elem.
        CONTINUE.
      ENDIF.

      "Assign inbound value for move
      ASSIGN is_flds_strcs-value->* TO <lv_value>.

      "Convert value into output format
      TRY.
          zcl_ca_conv=>internal_2_external(
                                      EXPORTING
                                        internal_value = <lv_value>
                                      IMPORTING
                                        external_value = lv_value ).

        CATCH zcx_ca_conv
              zcx_ca_intern.
          CONTINUE.
      ENDTRY.

      "Replace symbol by parameter value
      lv_symbol = create_symbol( is_flds_strcs-name ).
      REPLACE FIRST OCCURRENCE OF lv_symbol
                         IN TABLE ct_text
                             WITH lv_value IN CHARACTER MODE.
    ENDLOOP.
  ENDMETHOD.                    "replace_single_symbols


  METHOD replace_table_symbols.
    "-----------------------------------------------------------------*
    "   Replace table symbols by output of requested table data
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lr_text         TYPE REF TO data,
      ls_table        TYPE zca_s_table_in_text,
      lv_symbol       TYPE string,
      ls_match_result TYPE match_result,
      lx_error        TYPE REF TO cx_root.

    FIELD-SYMBOLS:
      <lt_text>            TYPE STANDARD TABLE.

    "Leave, if no table data are available
    IF it_tables IS INITIAL.
      RETURN.
    ENDIF.

    "Since the CHANGING parameter CT_TEXT can have different types
    "create a table object dynamically to collect the table lines.
    TRY.
        CREATE DATA lr_text LIKE ct_text.
        ASSIGN lr_text->* TO <lt_text>.

      CATCH cx_sy_create_data_error INTO lx_error.
        "Error occurred while creating a data object
        RAISE EXCEPTION TYPE zcx_ca_intern
          EXPORTING
            textid   = zcx_ca_intern=>data_creation_failed
            previous = lx_error.
    ENDTRY.

    "Prepare each table and replace the corresponding symbol
    LOOP AT it_tables INTO ls_table.
      "Search for table symbol = #table_name#
      CLEAR ls_match_result.
      lv_symbol = create_symbol( ls_table-name ).
      FIND FIRST OCCURRENCE OF lv_symbol
                      IN TABLE ct_text IN CHARACTER MODE
                       RESULTS ls_match_result.
      IF sy-subrc NE 0.
        "Try next entry if symbol wasn't found
        CONTINUE.
      ENDIF.

      ls_table = complete_table_defaults( ls_table ).

      "Prepare table lines
      prepare_table_lines(
                     IMPORTING
                       et_text  = <lt_text>
                     CHANGING
                       cs_table = ls_table ).

      "Delete line with symbol to insert table lines instead
      DELETE ct_text INDEX ls_match_result-line.

      IF <lt_text> IS NOT INITIAL.
        "Insert prepared tables lines into text
        INSERT LINES OF <lt_text> INTO  ct_text
                                  INDEX ls_match_result-line.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.                    "replace_table_symbols


  METHOD set_html_tag_font.
    "-----------------------------------------------------------------*
    "   Enclose current mail lines with font tags -> face/font size
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      ls_mail_text          TYPE soli.

    "Leave, if no DOM objects requested
    IF ms_text_font-without_head_body EQ c_true.
      RETURN.
    ENDIF.

    "Set style tag to switch between the font
    CASE iv_use_prop_font.
      WHEN c_true.
        "Set   p r o p o r t i o n a l   font and size
        "Set tag and replace font and font size by defined values.
        "Tag is: <font face="&1,&2" size="&3">
        ls_mail_text-line = cs_htmltag-open_font.
        "Set requested font
        REPLACE FIRST OCCURRENCE OF '&1'
                                 IN ls_mail_text-line
                               WITH ms_text_font-font_prop IN CHARACTER MODE.
        CASE ms_text_font-fonttype_prop.
          WHEN c_fonttype_prop.
            "Set generic font, if requested font does not exist on users desktop
            REPLACE FIRST OCCURRENCE OF '&2'
                                     IN ls_mail_text-line
                                   WITH 'sans-serif' IN CHARACTER MODE ##no_text.
          WHEN c_fonttype_monospc.
            "Set generic font, if requested font does not exist on users desktop
            REPLACE FIRST OCCURRENCE OF '&2'
                                     IN ls_mail_text-line
                                   WITH 'monospace' IN CHARACTER MODE ##no_text.
        ENDCASE.
        "Set font size in pt
        REPLACE FIRST OCCURRENCE OF '&3'
                                 IN ls_mail_text-line
                               WITH ms_text_font-fontsize_prop IN CHARACTER MODE.
        INSERT ls_mail_text INTO ct_mail_text INDEX 1.

      WHEN c_false.
        "Set   m o n o s p a c e d   font and size
        "Set tag and replace font and font size by defined values.
        "Tag is: <font face="&1,&2" size="&3">
        ls_mail_text-line = cs_htmltag-open_font.
        "Set requested font
        REPLACE FIRST OCCURRENCE OF '&1'
                                 IN ls_mail_text-line
                               WITH ms_text_font-font_monospc IN CHARACTER MODE.
        CASE ms_text_font-fonttype_monospc.
          WHEN c_fonttype_prop.
            "Set generic font, if requested font does not exist on users desktop
            REPLACE FIRST OCCURRENCE OF '&2'
                                     IN ls_mail_text-line
                                   WITH 'sans-serif' IN CHARACTER MODE ##no_text.
          WHEN c_fonttype_monospc.
            "Set generic font, if requested font does not exist on users desktop
            REPLACE FIRST OCCURRENCE OF '&2'
                                     IN ls_mail_text-line
                                   WITH 'monospace' IN CHARACTER MODE ##no_text.
        ENDCASE.
        "Set font size in pt
        REPLACE FIRST OCCURRENCE OF '&3'
                                 IN ls_mail_text-line
                               WITH ms_text_font-fontsize_monospc IN CHARACTER MODE.
        INSERT ls_mail_text INTO ct_mail_text INDEX 1.
    ENDCASE.

    "Set closing style tag
    ls_mail_text-line = cs_htmltag-close_font.    "= </font>
    APPEND ls_mail_text TO ct_mail_text.


* ! ! ! !    IV_CLOSE_LAST has default FALSE and is nowhere passed with another value and
* ! ! ! !    thus the commented part was never executed. ==>  Why? What was it for?
*    "Close last tag, if requested and set proportional font only if monospaced font was requested
*    IF iv_close_last    EQ abap_true AND
*       iv_use_prop_font EQ abap_false.
*      "Close last style, mostly proportional font
*      ls_mail_text-line = cs_htmltag-close_font.    "= </font>
*      INSERT ls_mail_text INTO ct_mail_text INDEX 1.
*
*      "Set proportional font
*      CLEAR ls_mail_text.
*      "Set tag and replace font and font size by defined values.
*      "Tag is: <font face="&1,&2" size="&3">
*      ls_mail_text-line = cs_htmltag-open_font.
*
*      REPLACE FIRST OCCURRENCE OF '&1'
*                               IN ls_mail_text-line
*                             WITH ms_text_font-font_prop IN CHARACTER MODE.
*      "Set generic font, if requested font does not exist on users desktop
*      REPLACE FIRST OCCURRENCE OF '&2'
*                               IN ls_mail_text-line
*                             WITH 'sans-serif' IN CHARACTER MODE ##no_text.
*      "Set font size in pt
*      REPLACE FIRST OCCURRENCE OF '&3'
*                               IN ls_mail_text-line
*                             WITH ms_text_font-fontsize_prop IN CHARACTER MODE.
*      APPEND ls_mail_text TO ct_mail_text.
*    ENDIF.
  ENDMETHOD.                    "set_html_tag_font


  METHOD set_html_tag_for_link.
    "-----------------------------------------------------------------*
    "   Set tags around links that they recognized as links
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lv_name              TYPE zca_d_name_link.

    CASE ms_text_font-doc_class.
      WHEN c_docclass_raw.
        "S A P   o f f i c e   t e x t
        rv_link = iv_link.

      WHEN c_docclass_htm.
        "H T M L   t e x t
        "Set a link name if no name is passed
        IF iv_name IS INITIAL.
          lv_name = 'Link'(lnk).
        ELSE.
          lv_name = iv_name.
        ENDIF.

        "Set link name w/o tag as link if no link is available
        IF iv_link IS INITIAL.
          rv_link = lv_name.
        ELSE.
          CONCATENATE '<a href="'           "Tag for link
                      iv_link
                      '" target="_blank">'  "open in new window
                      lv_name
                      '</a>'               INTO rv_link ##no_text.
        ENDIF.
    ENDCASE.
  ENDMETHOD.                    "set_html_tag_for_link


  METHOD set_html_tag_intro_n_body.
    "-----------------------------------------------------------------*
    "   Enclose completed mail in tags for HTML declaration and body
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      ls_mail_text    TYPE soli,
      lv_css_element  TYPE text20,
      lv_border_style TYPE text20.

    "This routine sets completion of the body and the complete header definition
    "from botton to top, except the CSS definition for the tables and the closing
    "element of the HTML document.

    "Leave, if no DOM objects requested
    IF ms_text_font-without_head_body EQ c_true.
      RETURN.
    ENDIF.

    "Set tag and replace font and font size by defined values.
    "Tag is:  <body>
    ls_mail_text-line = cs_htmltag-open_body.
    INSERT ls_mail_text INTO ct_mail_text INDEX 1.
    "Tag is:  </head>
    ls_mail_text-line = cs_htmltag-close_head.
    INSERT ls_mail_text INTO ct_mail_text INDEX 1.

    ls_mail_text-line = |<meta charset="{ ms_text_font-charset }">|.
    INSERT ls_mail_text INTO ct_mail_text INDEX 1.

    "Tag is:  <head>
    ls_mail_text-line = cs_htmltag-open_head.
    INSERT ls_mail_text INTO ct_mail_text INDEX 1.
    "Tag is:  <html>
    ls_mail_text-line = cs_htmltag-open_html.
    INSERT ls_mail_text INTO ct_mail_text INDEX 1.

    ls_mail_text-line = |<!doctype html>| ##no_text.
    INSERT ls_mail_text INTO ct_mail_text INDEX 1.

    "Tag is:  </body>
    ls_mail_text-line = cs_htmltag-close_body.
    APPEND ls_mail_text TO ct_mail_text.
    "Tag is:  </html>
    ls_mail_text-line = cs_htmltag-close_html.
    APPEND ls_mail_text TO ct_mail_text.
  ENDMETHOD.                    "set_html_tag_intro_n_body


  METHOD set_html_tag_line_break.
    "-----------------------------------------------------------------*
    "   Set line break tag at end of each text line
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      ls_mail_text          TYPE soli.

    "Leave, if no HTML is requestd
    IF ms_text_font-doc_class NE c_docclass_htm.
      RETURN.
    ENDIF.

    "Complete each line with a line break tag, excepting style tags
    LOOP AT ct_mail_text INTO  ls_mail_text
                         WHERE line NS cs_htmltag-line_break
                           AND line NP '<+capt*'
                           AND line NP '<capt*'
                           AND line NP '<+div*'
                           AND line NP '<div*'
                           AND line NP '<+font*'
                           AND line NP '<font*'
                           AND line NP '<+t*'
                           AND line NP '<t*'
*                           AND line NP '<+tbody*'
*                           AND line NP '<tbody*'
*                           AND line NP '<+ta*'
*                           AND line NP '<ta*'
*                           AND line NP '*<+tr*'
*                           AND line NP '<tr*'
*                           AND line NP '*<+td*'
*                           AND line NP '<td*'
                           AND line NP '*<+p*'
                           AND line NP '<p*' ##no_text.
      CONCATENATE ls_mail_text-line
                  cs_htmltag-line_break INTO ls_mail_text-line IN CHARACTER MODE.   "<br>
      MODIFY ct_mail_text FROM ls_mail_text.
    ENDLOOP.
  ENDMETHOD.                    "set_html_tag_line_break


  METHOD set_html_tag_table.
    "-----------------------------------------------------------------*
    "   Enclose table data with table and table record tags
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lt_mail_text         TYPE soli_tab,
      ls_mail_text         TYPE soli,
      lv_col_distance      TYPE n LENGTH 2,
      lv_open_tag          TYPE its_tag,
      lv_close_tag         TYPE its_tag,
      lv_strg_len          TYPE i,
      lv_strg_pos          TYPE i,
      lv_strg_pos_decimals TYPE i,
      lv_last_was_amnt     TYPE abap_bool,
      lv_strg_line         TYPE c LENGTH 2000.

    "Leave, if no HTML is requestd
    IF ms_text_font-doc_class NE c_docclass_htm.
      RETURN.
    ENDIF.

    "Copy current text to local, for recopying inclusive rec tags
    lt_mail_text = ct_mail_text.
    CLEAR ct_mail_text.

    DATA(lv_style_table_n_cells)       = create_style_for_table_n_cells( is_table ).
    DATA(lv_style_cell_right_align)    = enhance_cell_style_align_right( is_table ).
    DATA(lv_len_style_add_right_align) = strlen( lv_style_cell_right_align ) + 5.

    "To provide at least a working padding for the task description in the Fiori My Inbox
    ls_mail_text-line = condense( |<table { lv_style_table_n_cells }>| ) ##no_text.
    APPEND ls_mail_text TO ct_mail_text.

    "Set table record tags
    LOOP AT lt_mail_text INTO ls_mail_text.
      "Set column tag depending on current area
      IF ls_mail_text-line CS cs_htmltag-open_tab_hdr.
        lv_open_tag  = cs_htmltag-open_tab_hdr_col.
        lv_close_tag = cs_htmltag-close_tab_hdr_col.

      ELSEIF ls_mail_text-line CS cs_htmltag-open_tab_body.
        lv_open_tag  = cs_htmltag-open_tab_data_col.
        lv_close_tag = cs_htmltag-close_tab_data_col.
      ENDIF.

      IF ls_mail_text-line(1) EQ '<'  AND
         ls_mail_text-line    NS '>#' ##no_text.
        "Already tagged lines are used as they are
        APPEND ls_mail_text TO ct_mail_text.

      ELSE.
        lv_strg_line     = lv_open_tag(4).
        lv_strg_pos      = 4.
        lv_last_was_amnt = abap_false.

        DO c_length_mail TIMES.
          IF ls_mail_text-line(1) EQ space.
            "Move nothing - add only one for position
            ADD 1 TO lv_strg_pos.

          ELSEIF ls_mail_text-line(1) EQ c_coldelim_hash.
            "If no further data available set only a "close table data"
            IF ls_mail_text-line+1 IS INITIAL.
              lv_strg_line+lv_strg_pos = lv_close_tag(5).
              lv_strg_line = provide_cell_with_style( iv_mail_line           = lv_strg_line
                                                      iv_style_table_n_cells = lv_style_table_n_cells ).
              APPEND lv_strg_line TO ct_mail_text.
              CLEAR: lv_strg_line, lv_strg_pos.

            ELSE.
              "... otherwise close and open for next value
              lv_strg_line+lv_strg_pos = lv_close_tag(5).
              lv_strg_line = provide_cell_with_style( iv_mail_line           = lv_strg_line
                                                      iv_style_table_n_cells = lv_style_table_n_cells ).
              APPEND lv_strg_line TO ct_mail_text.
              CLEAR: lv_strg_line, lv_strg_pos.
              "If a '$$' follows a numeric value it should be aligned right
              IF ls_mail_text-line+1(2) EQ '$$'          AND
                 ls_mail_text-line+3(1) CO '1234567890' ##no_text.
                "With addition  ... text-align: right">.
                lv_strg_line+lv_strg_pos = condense( |<td { lv_style_cell_right_align }>| ) ##no_text.
                ADD lv_len_style_add_right_align TO lv_strg_pos.
                "delete #$ -> the second $ is deleted after ENDIF
                SHIFT ls_mail_text-line LEFT BY 2 PLACES.
                lv_last_was_amnt = abap_true.

              ELSE.
                lv_strg_line+lv_strg_pos = lv_open_tag(4).
                ADD 4 TO lv_strg_pos.
              ENDIF.
            ENDIF.

          ELSEIF ls_mail_text-line(2) EQ '+#'   AND
                 lv_last_was_amnt     EQ abap_true.
            "If the last value was an amount and has a sign and was positive
            "attach non-breaking space
            lv_strg_pos_decimals = strlen( lv_strg_line ) - 2.
*            IF lv_strg_line+lv_strg_pos_decimals(2) CO '1234567890' ##no_text.
*              lv_strg_line+lv_strg_pos(6) = '&nbsp;' ##no_text.
*              ADD 6 TO lv_strg_pos.
*            ENDIF.
            SHIFT ls_mail_text-line LEFT BY 1 PLACES.  "delete +
            "... and close the tag
            lv_strg_line+lv_strg_pos = lv_close_tag(5).
            lv_strg_line = provide_cell_with_style( iv_mail_line           = lv_strg_line
                                                    iv_style_table_n_cells = lv_style_table_n_cells ).
            APPEND lv_strg_line TO ct_mail_text.
            CLEAR: lv_strg_line, lv_strg_pos.
            lv_last_was_amnt = abap_false.

            "Was it NOT the last column? Then open the next table data tag
            IF ls_mail_text-line+1 IS NOT INITIAL.
              lv_strg_line+lv_strg_pos = lv_open_tag(4).
              ADD 4 TO lv_strg_pos.
            ENDIF.

          ELSE.
            "Attach current digit to line
            lv_strg_line+lv_strg_pos = ls_mail_text-line(1).
            ADD 1 TO lv_strg_pos.
          ENDIF.

          SHIFT ls_mail_text-line LEFT BY 1 PLACES.

          IF ls_mail_text-line IS INITIAL.
            EXIT.
          ENDIF.
        ENDDO.
      ENDIF.
    ENDLOOP.

    "Tag is:  </table>
    ls_mail_text-line = cs_htmltag-close_table.
    APPEND ls_mail_text TO ct_mail_text.
  ENDMETHOD.                    "set_html_tag_table


  METHOD set_text_type_n_font.
    "-----------------------------------------------------------------*
    "   Set text type, font and font size, default values if necessary
    "-----------------------------------------------------------------*
    "Set IMPORT parameter into global structure
    ms_text_font = is_text_font.

    "Set default text type
    IF ms_text_font-doc_class NE c_docclass_raw AND
       ms_text_font-doc_class NE c_docclass_htm.
      ms_text_font-doc_class = c_docclass_htm.
    ENDIF.

    "Set with hint for output in test system
    IF ms_text_font-without_hint_test CN '01'.
      ms_text_font-without_hint_test = c_false.
    ENDIF.

    "Leave if no HTML text type is requested
    IF ms_text_font-doc_class NE c_docclass_htm.
      ms_text_font-without_head_body = c_true.
      RETURN.
    ENDIF.

    "Set defaults if no font relevant values available
    "P r o p o r t i o n a l
    IF ms_text_font-font_prop IS INITIAL.
      ms_text_font-font_prop = c_def_font_prop.
    ENDIF.
    IF ms_text_font-font_prop IS INITIAL.
      ms_text_font-font_prop = c_def_font_prop.
    ENDIF.
    IF ms_text_font-fontsize_prop IS INITIAL.
      ms_text_font-fontsize_prop = c_def_fontsize_prop.
    ENDIF.
    IF ms_text_font-fonttype_prop IS INITIAL.
      ms_text_font-fonttype_prop = c_fonttype_prop.
    ENDIF.
    "M o n o s p a c e d
    IF ms_text_font-font_monospc IS INITIAL.
      ms_text_font-font_monospc = c_def_font_monospc.
    ENDIF.
    IF ms_text_font-fontsize_monospc IS INITIAL.
      ms_text_font-fontsize_monospc = c_def_fontsize_monospc.
    ENDIF.
    IF ms_text_font-fonttype_monospc IS INITIAL.
      ms_text_font-fonttype_monospc = c_fonttype_monospc.
    ENDIF.

    "Set character set
    IF ms_text_font-charset IS INITIAL.
      TRY.
          ms_text_font-charset = cl_abap_codepage=>current( http_name = abap_true ).

        CATCH cx_parameter_invalid.
          ms_text_font-charset = 'utf-8' ##no_text.
      ENDTRY.
    ENDIF.

    "Set with DOM (tags for head and body)
    IF ms_text_font-without_head_body CN '01'.
      ms_text_font-without_head_body = c_false.
    ENDIF.
  ENDMETHOD.                    "set_text_type_n_font


  METHOD unlock_source.
    "-----------------------------------------------------------------*
    "   Release source object
    "-----------------------------------------------------------------*
    CALL FUNCTION 'DEQUEUE_ESRDIRE'
      EXPORTING
        mode_trdir = 'X'
        name       = c_name_incl_ddic_def
        _scope     = '1'.
  ENDMETHOD.                    "unlock_source

ENDCLASS.
