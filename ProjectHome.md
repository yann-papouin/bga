BGA is a set of tools to help modders of Battlefield 1942 and Battlefield Vietnam manipulating their files.

These tools are commonly used to extract/import data from/into rfa (battlefield archive file), quickly searching for a file in an archive, previewing common files like images, scripts, models, heightmaps.

# BGA Actual feature list #

  * Param read support (allow opening directly a RFA archive from the shell)
  * RFA archive file association (go to Help->Tools->Associate RFA files with BGA)
  * Recent opened/saved file list
  * Real compression support
  * Import any files into RFA archive
  * Import and compress any files into RFA archive
  * Export any files from RFA archive
  * Export and decompress any files from RFA archive
  * Direct edit of any files without having to extract them manually
  * Batch import/export (with recent dir used support)
  * Drag&drop support
  * Quick save operation even with compressed data
  * Defrag operation (due to quick save)
  * Search bar to easily find a file/folder
  * Built-in picture preview (.tga, .dds, .jpg, .bmp)
  * Built-in Standardmesh (.sm) model preview
  * Built-in Heightmap (.raw) + Terrain (.con) preview
  * Previews are with textures if they exists in the same archive
  * Custom file association (Edit->Settings)
  * Software update notification

# BGA Future #

  * ## Virtual file system support ##
The purpose of a virtual file system is to allow real map preview. A database will be built for each mods and their ancestors, that way we will know wich file need to be patched on the fly. The virtual filesystem engine will be able to detect any archive modifications and will be automatically updated.

  * ## Script parser ##
The Battlefield engine uses some scripts (.con) to init data. The engine is built upon a class system so all we need to do is rebuilt this system with a script parser. When finalized, all data loading/saving will be transparent, without any destructive effects (like erasing user custom scripts).

![http://img341.imageshack.us/img341/6331/sshot13d.png](http://img341.imageshack.us/img341/6331/sshot13d.png)