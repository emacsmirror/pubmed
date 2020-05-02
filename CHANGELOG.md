# Changelog
## [0.5] - 2020-05-02
### Added
- Font-locking is used for more consistent hightlighting.

### Removed
- Customization of faces is removed because of font-locking

### Fixed
- Added a new mode for showing entries, because of different indentation for query results

## [0.4.3] - 2020-04-26
### Fixed
- Set only one variable with setq-local for backward compatibility

## [0.4.2] - 2020-03-15
### Added
- Marks are remembered after sorting

### Fixed
- Multiple buffers don't interfere with each other because of buffer-local variables

## [0.4] - 2020-03-12
### Added
- Major interface change (using EWOC instead of tabulated-list-mode)
- Added sorting options
- Customize default sorting with a menu

### Fixed
- Preserve the order of the results if the count exceeds 500
- Minor bugfixes

## [0.3.3] - 2020-02-21
### Fixed
- Fix allowed sort orders
- Some refactoring

## [0.3.2] - 2020-02-08
### Fixed
- Changed http to https to reflect change in Sci-Hub url

## [0.3.1] - 2019-12-20
### Fixed
- Hotfix in Sci-Hub fulltext function

## [0.3] - 2019-12-04
### Added
- Added Springer fulltext function
- Added Dissemin fulltext function

### Fixed
- Fixed typos
- Removed workaround for a bug in Open Access Button service that is fixed upstream

## [0.2.3] - 2019-12-02
### Added
- Added menu

## [0.2.2] - 2019-12-02
### Fixed
- Cleanup
- Fixed missing dependency

### Added
- Updated README

## [0.2.1] - 2019-05-02
### Fixed
- Multiple bugfixes in BibTeX keypattern functions

## [0.2] - 2019-05-01
### Added
- Support summaries of books and chapters
- Added Open Access Button fulltext function
- Added BibTeX keypattern functions
- Make BibTeX citation keys unique by adding a letter
- Writing BibTeX references appends in stead of overwrites by default

## [0.1.1] - 2019-04-28
### Added
- Now available from MELPA

## [0.1] - 2019-03-26
- Initial Release
