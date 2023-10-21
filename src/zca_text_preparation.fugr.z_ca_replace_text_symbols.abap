"! <p class="shorttext synchronized" lang="en">Common object: Replace text symbols/p>
"!
"! <p>The replacement of SAP script symbols must be executed in a function group or program because other objects,
"! e. g. like classes, are not selectable in transaction SO10. Furthermore is it necessary to maintain the used
"! DDIC table definitions in a different object than the main class. Otherwise it would not be possible to generate
"! the changed code and use it immediately.</p>
"!
"! @parameter text_module_header      | <p class="shorttext synchronized" lang="en">SAPscript: Text Header</p>
"! @parameter fields_n_structures     | <p class="shorttext synchronized" lang="en">Field and structure values</p>
"! @parameter text_module_lines       | <p class="shorttext synchronized" lang="en">Text module lines where the symbols should be replaced</p>
"! @raising   zcx_ca_text_preparation | <p class="shorttext synchronized" lang="en">Common exception: While preparing text module</p>
FUNCTION z_ca_replace_text_symbols.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(TEXT_MODULE_HEADER) TYPE  THEAD
*"     REFERENCE(FIELDS_N_STRUCTURES) TYPE  ZCA_TT_PARAMS
*"  CHANGING
*"     REFERENCE(TEXT_MODULE_LINES) TYPE  TLINE_TAB
*"  RAISING
*"      ZCX_CA_TEXT_PREPARATION
*"----------------------------------------------------------------------
  "Set transmitted data into global structure definitions
  PERFORM set_inbound_data_into_global USING fields_n_structures.

  "Replace standard symbols in text
  PERFORM replace_standard_symbols USING    text_module_header
                                   CHANGING text_module_lines.
ENDFUNCTION.
