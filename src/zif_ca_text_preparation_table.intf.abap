"! <p class="shorttext synchronized" lang="en">CA-TBX: Text preparation of table values</p>
INTERFACE zif_ca_text_preparation_table PUBLIC.


* i n s t a n c e   a t t r i b u t e s
  DATA:
*   o b j e c t   r e f e r e n c e s
    "! <p class="shorttext synchronized" lang="en">Technical description of the table</p>
    techn_table      TYPE REF TO cl_abap_tabledescr READ-ONLY,

*   d a t a   r e f e r e n c e s
    "! <p class="shorttext synchronized" lang="en">Text preparation: Control + font settings for all types</p>
    control_settings TYPE REF TO zca_s_text_prep_ctrl_settings READ-ONLY,
    "! <p class="shorttext synchronized" lang="en">Control settings for table preparation</p>
    settings         TYPE REF TO zca_s_table_in_text READ-ONLY.

**   t a b l e s
*      "! <p class="shorttext synchronized" lang="en">Description</p>
*      mt_...               TYPE x..
*
**   s t r u c t u r e s
*      "! <p class="shorttext synchronized" lang="en">Description</p>
*      ms_...               TYPE x..
*
**   s i n g l e   v a l u e s
*      "! <p class="shorttext synchronized" lang="en">Description</p>
*      mv_...               TYPE x..

* i n s t a n c e   m e t h o d s
  METHODS:
    "! <p class="shorttext synchronized" lang="en">Description</p>
    "!
    "! @parameter result                  | <p class="shorttext synchronized" lang="en">Prepared table rows</p>
    "! @raising   zcx_ca_text_preparation | <p class="shorttext synchronized" lang="en">CA-TBX exception: While preparing text module</p>
    "! @raising   zcx_ca_conv             | <p class="shorttext synchronized" lang="en">Common exception: Conversion failed</p>
    prepare_table_rows
      RETURNING
        VALUE(result) TYPE zca_tt_text_lines
      RAISING
        zcx_ca_text_preparation
        zcx_ca_conv,

    "! <p class="shorttext synchronized" lang="en">Set default values for non-provided table settings</p>
    "!
    "! @raising   zcx_ca_text_preparation | <p class="shorttext synchronized" lang="en">CA-TBX exception: While preparing text module</p>
    set_defaults_in_table_settings
      RAISING
        zcx_ca_text_preparation.
ENDINTERFACE.
