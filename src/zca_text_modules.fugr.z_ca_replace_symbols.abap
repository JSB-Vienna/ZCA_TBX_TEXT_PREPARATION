"! <p class="shorttext synchronized" lang="en">Common object: Replace text symbols/p>
FUNCTION Z_CA_REPLACE_SYMBOLS.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IS_THEAD) TYPE  THEAD
*"     REFERENCE(IT_FLDS_STRCS) TYPE  ZCA_TT_PARAMS
*"  CHANGING
*"     REFERENCE(CT_TEXT) TYPE  TLINE_TAB
*"  RAISING
*"      ZCX_CA_TEXT_MODULE
*"----------------------------------------------------------------------
  "Set transmitted data into global structure definitions
  PERFORM set_inbound_data_into_global USING it_flds_strcs.

  "Replace standard symbols in text
  PERFORM replace_standard_symbols USING    is_thead
                                   CHANGING ct_text.
ENDFUNCTION.
