

#RFA file format quick description

    Introduction
    RFA file structure
    RFA file fragmentation

Introduction

The RFA file format is a standalone file system, this page will try to explain you how data is hierarchised. In following explanations we are talking about a RFA archive that contains 5 files named a, b, c, d and e.
RFA file structure

Data and entries are stored in the same file, the RFA archive.

Data are not directly readable, because you need to know how they are organized. This organization is given by the entries table that can be found at the end of the file.

Two other informations exist outside the data and entries structure, they are the data size and the quantity of entries that can be found in the table. With them, we can directly move our reading cursor at the start of the table and identify data.

Identifying data is made when parsing each entry. In each of them we can know the path of the file (eg:"bf1942/levels/dc_lostvillage/detail.dds"), it's compressed size (that will be different from the uncompressed size only if the data has been compressed before), it's uncompressed size, and finally the offset

The offset help us to know where data start, and size where it finished. Note that compressed data is a little more complex as data is divided in segments and next compressed.

RFA file fragmentation

BGA includes a new thing that doesn't exists in other tools, it is the fragmentation. Because today hard drive space is so huge but access delay are as long as before, fragmentation will cost a small loss of space but can be recovered by a defrag operation.

After having edited the "c" file, we save modifications, the new "c" file in inserted at the end of the data part and the "c" entry is updated with the new offset and size informations. In that case, the space used by the old "c" file is lost. It is due by the fact that there is no more entry referencing it. Actually you could thought that we just could had to shift all data after the old "c" file to avoid fragmentation, but this operation is too long for a daily use of BGA, that's why we are shifting only a small part (entries table).

But there is still one case when no data is loss, it's when the edited file is at that last position in the data part. Entries table is just shifted and the old file at last position is overridden.

But if we decide to edit a new file and save modifications, then "c" is no more at the last position.

So, after a third edit and save of the "c" file, we have lost space again.

To recover lost space, you just have to launch a "defrag" operation.

In BGA, the "defrag" can be executed from the "File" menu, it is like a save operation but all lost space is recovered. Note that a "Save As" operation automatically execute a "defrag".
