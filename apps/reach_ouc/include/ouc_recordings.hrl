
%% The name of the collection where to store recording files details.
-define(REC_META_COLL, <<"recordings">>).

%% The name of the Mongo bucket where to store recording files.
%% This bucket will contain two collections: chunks and files.
-define(REC_GFS_BUCKET, <<"fs">>).

%% The name of the collection that contains the recording files chunks.
-define(REC_GFS_CHUNKS_COLL, <<"chunks">>).

%% The name of the collection where the files metadata is stored by Gridfs.
-define(REC_GFS_FILES_COLL, <<"files">>).

%% The time to wait before uploading the file to Mongo.
%% Used to be sure that Freeswitch stopped the file recording.
-define(REC_UPLOAD_DELAY, 3000).

%% The size of file recording chunks
-define(REC_CHUNK_SIZE, 131072).
