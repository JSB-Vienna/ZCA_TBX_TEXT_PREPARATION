"! <p class="shorttext synchronized" lang="en">CA-TBX: Constants for text module preparation</p>
INTERFACE zif_ca_c_text_modules PUBLIC.
* i n t e r f a c e s
  INTERFACES:
    zif_ca_c_doc_class.

* a l i a s e s
  ALIASES:
*   Document / text classes
    c_docclass_htm         FOR  zif_ca_c_doc_class~c_docclass_htm,
    c_docclass_raw         FOR  zif_ca_c_doc_class~c_docclass_raw.

* c o n s t a n t s
  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">Default font size: for monospace font</p>
    c_def_fontsize_monospc       TYPE zca_d_fontsize_monospc VALUE '3'  ##no_text,
    "! <p class="shorttext synchronized" lang="en">Default font size: for proportional font</p>
    c_def_fontsize_prop          TYPE zca_d_fontsize_prop    VALUE '3'  ##no_text,

    "! <p class="shorttext synchronized" lang="en">Default font: for text</p>
    c_def_font_monospc           TYPE zca_d_font_monospc     VALUE 'Courier New'  ##no_text,
    "! <p class="shorttext synchronized" lang="en">Default font: for HTML</p>
    c_def_font_prop              TYPE zca_d_font_prop        VALUE 'Arial'  ##no_text,

    "! <p class="shorttext synchronized" lang="en">Font type: Monospace</p>
    c_fonttype_monospc           TYPE zca_d_font_type        VALUE 'M'  ##no_text,
    "! <p class="shorttext synchronized" lang="en">Font type: Proportional</p>
    c_fonttype_prop              TYPE zca_d_font_type        VALUE 'P'  ##no_text,

    "! <p class="shorttext synchronized" lang="en">Frame style: Inherit</p>
    c_table_desc_alignm_center  TYPE zca_d_table_desc_alignment VALUE '0'  ##no_text,
    "! <p class="shorttext synchronized" lang="en">Frame style: Left</p>
    c_table_desc_alignm_left    TYPE zca_d_table_desc_alignment VALUE '1'  ##no_text,
    "! <p class="shorttext synchronized" lang="en">Frame style: Right</p>
    c_table_desc_alignm_right   TYPE zca_d_table_desc_alignment VALUE '2'  ##no_text,
    "! <p class="shorttext synchronized" lang="en">Frame style: Inherit</p>
    c_table_desc_alignm_inherit TYPE zca_d_table_desc_alignment VALUE '9'  ##no_text,

    "! <p class="shorttext synchronized" lang="en">Frame style: None (no frame/border)</p>
    c_framestyle_none            TYPE zca_d_frame_style      VALUE '0'  ##no_text,
    "! <p class="shorttext synchronized" lang="en">Frame style: Solid</p>
    c_framestyle_solid           TYPE zca_d_frame_style      VALUE '1'  ##no_text,
    "! <p class="shorttext synchronized" lang="en">Frame style: Ridge (like 3D)</p>
    c_framestyle_ridge           TYPE zca_d_frame_style      VALUE '2'  ##no_text,
    "! <p class="shorttext synchronized" lang="en">Frame style: Inherit from a higher element</p>
    c_framestyle_inherit         TYPE zca_d_frame_style      VALUE '9'  ##no_text,

    "! <p class="shorttext synchronized" lang="en">Frame width: Thin</p>
    c_framewidth_thin            TYPE zca_d_frame_width      VALUE '0'  ##no_text,
    "! <p class="shorttext synchronized" lang="en">Frame width: Medium</p>
    c_framewidth_medium          TYPE zca_d_frame_width      VALUE '1'  ##no_text,
    "! <p class="shorttext synchronized" lang="en">Frame width: Thick</p>
    c_framewidth_thick           TYPE zca_d_frame_width      VALUE '2'  ##no_text,
    "! <p class="shorttext synchronized" lang="en">Frame width: Inherit from a higher element</p>
    c_framewidth_inherit         TYPE zca_d_frame_width      VALUE '9'  ##no_text.
ENDINTERFACE.
