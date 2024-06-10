"! <p class="shorttext synchronized" lang="en">CA-TBX: Text module preparation</p>
INTERFACE zif_ca_text_preparation PUBLIC.
* i n s t a n c e   a t t r i b u t e s
  DATA:
*   o b j e c t   r e f e r e n c e s
    "! <p class="shorttext synchronized" lang="en">CA-TBX: Constants for numeric boolean flags</p>
    boolean             TYPE REF TO zcl_ca_c_numeric_boolean READ-ONLY,
    "! <p class="shorttext synchronized" lang="en">CA-TBX: Constants and value checks for select option tables</p>
    sel_options         TYPE REF TO zcl_ca_c_sel_options READ-ONLY,
    "! <p class="shorttext synchronized" lang="en">CA-TBX: Constants + value checks for text module preparation</p>
    tp_options          TYPE REF TO zcl_ca_c_text_preparation READ-ONLY,
    "! <p class="shorttext synchronized" lang="en">CA-TBX: Text preparation: Text in preparation for output</p>
    text_in_preparation TYPE REF TO zcl_ca_text_prepared_lines READ-ONLY,

*   t a b l e s
*      "! <p class="shorttext synchronized" lang="en">Range table with excluding or including output fields</p>
*      mra_output_flds        TYPE rsdsselopt_t,
    "! <p class="shorttext synchronized" lang="en">Name and value pairs for fields and structures</p>
    fields_n_structures TYPE zca_tt_params READ-ONLY,
    "! <p class="shorttext synchronized" lang="en">Links and their description</p>
    links               TYPE zca_tt_texts_n_links READ-ONLY,
    "! <p class="shorttext synchronized" lang="en">Table values</p>
    tables              TYPE zca_tt_tables_in_text READ-ONLY,

**   d a t a   r e f e r e n c e s
*      "! <p class="shorttext synchronized" lang="en">Description</p>
*      mr_...               TYPE REF TO x..
*
**   t a b l e s
*      "! <p class="shorttext synchronized" lang="en">Description</p>
*      mt_...               TYPE x..
*
*   s t r u c t u r e s
    "! <p class="shorttext synchronized" lang="en">Text preparation: Control + font settings for all types</p>
    control_settings    TYPE zca_s_text_prep_ctrl_settings READ-ONLY,

*   s i n g l e   v a l u e s
    "! <p class="shorttext synchronized" lang="en">Target type for text module preparation</p>
    preparation_type    TYPE zca_d_target_preparation_type READ-ONLY.

* i n s t a n c e   m e t h o d s
  METHODS:
    "! <p class="shorttext synchronized" lang="en">Get result as SOLI_TAB (SAP office format, e. g. for mails)</p>
    "!
    "! @parameter result | <p class="shorttext synchronized" lang="en">Prepared text as of line type SOLI (CHAR 255)</p>
    get_result_as_sap_office_tab DEFAULT FAIL
      RETURNING
        VALUE(result) TYPE soli_tab,

    "! <p class="shorttext synchronized" lang="en">Get result as SAP script module table (TLINE_TAB)</p>
    "!
    "! <p>The format of this result is the same as it is required for text modules or SAP script forms.</p>
    "!
    "! @parameter result                  | <p class="shorttext synchronized" lang="en">Prepared text as of line type TLINE (two fields!)</p>
    "! @raising   zcx_ca_text_preparation | <p class="shorttext synchronized" lang="en">CA-TBX exception: While preparing text module</p>
    get_result_as_sap_script_table DEFAULT FAIL
      RETURNING
        VALUE(result) TYPE tline_tab
      RAISING
        zcx_ca_text_preparation,

    "! <p class="shorttext synchronized" lang="en">Get result as stream (= STRING)</p>
    "!
    "! <p>This result is e. g. suitable for the usage within workflow task descriptions and is expected by e. g.
    "! the <strong><em>enhancement spot {@link /iwpgw/es_tgw_task_data} method
    "! {@link /iwpgw/if_tgw_task_data.METH:modify_task_description} parameter
    "! {@link /iwpgw/if_tgw_task_data.METH:modify_task_description.DATA:cv_description_html}</em></strong>.</p>
    "! <p>For the use with the mentioned enhancement spot you have to set the <strong><em>value WITHOUT_HEAD_BODY
    "! in {@link .METH:replace_n_transform.DATA:control_settings}</em></strong> to BOOLEAN->TRUE. This is because
    "! the workflow task description is already a complete HTML site and the prepared text lines are only a
    "! addition to it and needs therefore no HTML-head- and no HTML-body-statement. Otherwise it can irritate
    "! the presentation in the Fiori Inbox.</p>
    "!
    "! @parameter add_addition_if_missing | <p class="shorttext synchronized" lang="en">X = Add MAIL_HTML for task descriptions if not found</p>
    "! <p>Workflow task descriptions are maintained with SAP script. Like HTML the SAP script formatting symbols
    "! are using also the characters "<" and ">". This is why a specific first line has to be inserted to force
    "! that the SAP script parser is NOT executed which would destroy a HTML preparation.</p>
    "! <p>Using this flag it can be controlled whether the necessary line is checked and added if not found.</p>
    "! <p>
    "! @parameter result                  | <p class="shorttext synchronized" lang="en">Prepared text as of type STRING</p>
    get_result_as_stream DEFAULT FAIL
      IMPORTING
        add_addition_if_missing TYPE abap_boolean DEFAULT abap_false
      RETURNING
        VALUE(result)           TYPE string,

    "! <p class="shorttext synchronized" lang="en">Get result as workitem description table</p>
    "!
    "! <p>This result is a format that can be used as a table attribute in classic Business Objects (BO), e. g.
    "! to provide the task description with tables as output element, such like a approver list or purchase order
    "! items to be approved.</p>
    "!
    "! @parameter result                  | <p class="shorttext synchronized" lang="en">Prepared text as of line type TDLINE (CHAR 132)</p>
    "! @raising   zcx_ca_text_preparation | <p class="shorttext synchronized" lang="en">CA-TBX exception: While preparing text module</p>
    get_result_as_wi_descr_table DEFAULT FAIL
      RETURNING
        VALUE(result) TYPE htmltable
      RAISING
        zcx_ca_text_preparation,

    "! <p class="shorttext synchronized" lang="en">Replace variables and transform text into target format</p>
    "!
    "! <p>The parameters of this method are of structured types. To make sure that the user has to provide as less
    "! as possible settings the <strong><em>flag fields expect 0 and 1 (use attribute BOOLEAN)!</em></strong> This
    "! would not be possible using the values ABAP_FALSE and ABAP_TRUE.</p>
    "!
    "! <p>Furthermore <strong><em>take care</em></strong> that all the <strong><em>necessary data are accessible
    "! from the point where you call this method!</em></strong> This means that all variables must be defined
    "! locally in your calling method or must not be private or in an instance that is already cleared. There
    "! are further aspect to be respected.</p>
    "!
    "! <p>There are <strong>TWO</strong> types of symbols in use. The standard <strong>SAP script symbols</strong>
    "! like this <strong>&EKKO-EBELN(Z)&</strong> and the specific syntax for <strong>internal symbols</strong>
    "! of this text preparation tool like this <strong>#T_NOTES#</strong>. The first one, SAP script symbols,
    "! will only be replaced by values of structures, while the internal symbols can only be used for single
    "! values, internal tables and links (URL). For internal symbols must be a name provided, while it can be
    "! empty for structures used for SAP script symbols.</p>
    "!
    "! <p>Parameters or values that are not relevant for the requested preparation type will be ignored.
    "! E. g., for RAW (= plain text) the parameter LINKS makes no sense to replace.<p>
    "! <p>This method assumes that the <strong><em>{@link .METH:use_text_from}</em></strong> method has already
    "! been executed.</p>
    "! <p>
    "! @parameter control_settings     | <p class="shorttext synchronized" lang="en">Text preparation: Control + font settings for all types</p>
    "! <p>Common values to control the text preparation. As far as possible the structure is build from several
    "! substructures to express their purpose / assignment to a specific transformation type.</p>
    "! <ul>
    "! <li><strong><em>General settings - </em></strong>E. g., mark if you like to include a hint that documents
    "! that the document is created in a test system.</li>
    "! <li><strong><em>for HTML documents - </em></strong>Here you can set the font types and size to be used.</li>
    "! <li><strong><em>for RAW documents  - </em></strong>?</li>
    "! </ul>
    "! <p>
    "! @parameter fields_n_structures  | <p class="shorttext synchronized" lang="en">Name and value pairs for fields and structures</p>
    "! <p>This parameter has to be used when passing either specifically formated single values or structured
    "! values.</p>
    "!
    "! <p>If the structured type is defined in the DDIC this class enhances the top include of FG
    "! ZCA_TEXT_PREPARATION to make the structure fields accessible in <strong><em>transaction SO10</em></strong>.</p>
    "!
    "! <p><strong><em>Take notice</em></strong> that structured objects are <strong><em>only used</em></strong>
    "! for the <strong><em>SAP script replacements</em></strong> (requires the use of text module in transaction
    "! SO10)!! To display several fields of a structure use parameter TABLES and provide a table with a single
    "! row of content. There you have several options to control the output which is not given here.</p>
    "!
    "! <p>Regarding the usage of the <strong><em>field selection in transaction SO10:</em></strong><br>
    "! <strong><em>After the first execution</em></strong> of this class it is possible to use the functionality
    "! to insert <strong><em>program symbols (CTRL+F3)</em></strong> in the SO10 editor. If the function group
    "! is still not offered in the following popup, <strong><em>use 'Append Print Program'</em></strong>, enter
    "! the program name <strong><em>'SAPLZCA_TEXT_PREPARATION'</em></strong> and press 'Enter'. After that you
    "! can select the program in the first popup. The rest is self-explaining. For further details about how to
    "! format values in SAP script have a look <strong><em>here: BC SAP Script - Formatting Options.</em></strong><br>
    "! https://help.sap.com/viewer/59204ae5e0d745628df068a6ec7591b0/7.51.7/en-US/4e34031063de02c2e10000000a15822b.html</p>
    "! <p>
    "! @parameter tables               | <p class="shorttext synchronized" lang="en">Table values</p>
    "! <p>Each entry of this table represent one internal table with its controlling attributes. A lot of the
    "! attributes are only relevant for the transformation into HTML.</p>
    "! <p><strong><em>HINT to parameter column SIGN_FOR_FLDS</em></strong>: Here you can use well known characters
    "! 'I'nclude or 'E'xclude. But be aware when using 'E' that the named fields in T_OUTPUT_FLDS are only used
    "! to exclude them from the output and that it is not possible to give in an order for the rest of the fields.
    "! In the case you have much more inclusive than exclusive fields and need a different order than use either
    "! a specific definition for your needs or build a little routine that creates the list of needed fields.</p>
    "! <p>Furthermore you can pass an individual table header description. Which one you should provide depends
    "! on the output length of the column (mostly defined in the domain). For an output length lower equal 10
    "! use field <strong><em>SCRTEXT_S</em></strong>, lower equal 20 use <strong><em>SCRTEXT_M</em></strong>
    "! and for all others use <strong><em>SCRTEXT_L</em></strong>.</p>
    "! <p><strong><em>Reserve always a complete line</em></strong> in your text module for a table since the
    "! complete line with the corresponding symbol (e. g. #T_NOTES#) will replaced, irrespective what else the
    "! line contains.</p>
    "! <p>
    "! @parameter links                | <p class="shorttext synchronized" lang="en">Links and their description</p>
    "! <p>Within this parameter you can pass all necessary components to assemble HTML links. Add a short description
    "! in <strong><em>LINK_DESC</em></strong> or use a <strong><em>text module</em></strong> for a more detailed
    "! description. A <strong><em>text module should be preferred</em></strong> either if a longer description
    "! is necessary or if it is <strong><em>repetitively used in several documents</em></strong>. The description
    "! will be set above the link. <strong><em>Reserve always a complete line</em></strong>, like for tables too,
    "! since links are resolved internally as tables due to the fact that they can have a text module as
    "! description. Perhaps it will be possible in the future to replace variables in the description
    "! for a link.</p>
    "! <p>In <strong><em>LINK_NAME</em></strong> you can provide a short naming to hide the technical link, e. g.
    "! "Open my Fiori Inbox".</p>
    "! <p>
    "! @raising   zcx_ca_text_preparation | <p class="shorttext synchronized" lang="en">CA-TBX exception: While preparing text module</p>
    replace_n_transform DEFAULT FAIL
      IMPORTING
        control_settings    TYPE zca_s_text_prep_ctrl_settings OPTIONAL
        fields_n_structures TYPE zca_tt_params                 OPTIONAL
        tables              TYPE zca_tt_tables_in_text         OPTIONAL
        links               TYPE zca_tt_texts_n_links          OPTIONAL
      RAISING
        zcx_ca_text_preparation,

    "! <p class="shorttext synchronized" lang="en">Import text from table or text module</p>
    "!
    "! <p>If you use individual symbols for replacement, like tables or links, then provide these symbols in
    "! <strong><em>hashes, e. g. #T_NOTES#</em></strong>. These symbol names <strong><em>must be the same as
    "! provided in column NAME (here without hashes!)</em></strong> in the parameters {@link .METH:replace_n_transform.DATA:fields_n_structures},
    "! {@link .METH:replace_n_transform.DATA:tables} and {@link .METH:replace_n_transform.DATA:links} of
    "! <strong><em>method {@link .METH:replace_n_transform}</em></strong></p>
    "! <p>
    "! <p>The following lines are an example how a text could look alike:<br>
    "! <em>Dear #SALUTATION#,<br>
    "! we would like to inform you about open work items:<br>
    "! #T_TASK_LIST#<br>
    "! <br>
    "! You can open them using the following link:<br>
    "! #LINK_START_LAUNCHPAD#<br>
    "! <br>
    "! Best regards<br>
    "! Your SAP IT-Team</em><br>
    "! </p>
    "!
    "! @parameter text_module_key         | <p class="shorttext synchronized" lang="en">Text module key (has to be maintained in SO10)</p>
    "! @parameter text_lines              | <p class="shorttext synchronized" lang="en">Table that contains the text lines to be prepared</p>
    "! @raising   zcx_ca_text_preparation | <p class="shorttext synchronized" lang="en">CA-TBX exception: While preparing text module</p>
    use_text_from DEFAULT FAIL
      IMPORTING
        text_module_key TYPE stxh_key          OPTIONAL
        text_lines      TYPE zca_tt_text_lines OPTIONAL
      RAISING
        zcx_ca_text_preparation.

ENDINTERFACE.
