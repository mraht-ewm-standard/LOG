CLASS zial_apack_logging DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_apack_manifest.

    METHODS constructor.

ENDCLASS.


CLASS zial_apack_logging IMPLEMENTATION.

  METHOD constructor.

    if_apack_manifest~descriptor-group_id     = 'c-a-s.de'.
    if_apack_manifest~descriptor-artifact_id  = 'logging'.
    if_apack_manifest~descriptor-version      = '20.11.2024.001-rc'.
    if_apack_manifest~descriptor-git_url      = 'https://github.com/mraht-ewm-standard/LOGGING.git' ##NO_TEXT.

    if_apack_manifest~descriptor-dependencies = VALUE #(
        ( group_id    = 'c-a-s.de'
          artifact_id = 'aunit'
          version     = '17.02.2024.001-rc'
          git_url     = 'https://github.com/mraht-ewm-standard/AUNIT.git' ) ).

  ENDMETHOD.

ENDCLASS.
