# RFA file format quick description

## Introduction

The RFA file format is a standalone file system, this page will try to explain you how data is hierarchised. In following explanations we are talking about a RFA archive that contains 5 files named a, b, c, d and e.
RFA file structure

Data and entries are stored in the same file, the RFA archive.

![rect6720](https://github.com/user-attachments/assets/cc777a68-f331-4903-9930-cfe973110d2b)

Data are not directly readable, because you need to know how they are organized. This organization is given by the entries table that can be found at the end of the file.

![rect6718](https://github.com/user-attachments/assets/17ffa1d2-6a73-4a3a-a151-fcb0ab0accc9)

Two other informations exist outside the data and entries structure, they are the data size and the quantity of entries that can be found in the table. With them, we can directly move our reading cursor at the start of the table and identify data.

![rect6716](https://github.com/user-attachments/assets/c6174367-ce68-4588-ae97-9fd163961d32)

Identifying data is made when parsing each entry. In each of them we can know the path of the file (eg:"bf1942/levels/dc_lostvillage/detail.dds"), it's compressed size (that will be different from the uncompressed size only if the data has been compressed before), it's uncompressed size, and finally the offset

![rect6722](https://github.com/user-attachments/assets/7190fb18-33e9-41e0-86ad-d438612835d9)

The offset help us to know where data start, and size where it finished. Note that compressed data is a little more complex as data is divided in segments and next compressed.

![rect3266](https://github.com/user-attachments/assets/3e8c754e-a282-4532-849d-f89aad3f2c47)

## RFA file fragmentation

BGA includes a new thing that doesn't exists in other tools, it is the fragmentation. Because today hard drive space is so huge but access delay are as long as before, fragmentation will cost a small loss of space but can be recovered by a defrag operation.

![rect6726](https://github.com/user-attachments/assets/96fbeca5-de13-4906-a664-e5210092aade)

After having edited the "c" file, we save modifications, the new "c" file in inserted at the end of the data part and the "c" entry is updated with the new offset and size informations. In that case, the space used by the old "c" file is lost. It is due by the fact that there is no more entry referencing it. Actually you could thought that we just could had to shift all data after the old "c" file to avoid fragmentation, but this operation is too long for a daily use of BGA, that's why we are shifting only a small part (entries table).

![rect6728](https://github.com/user-attachments/assets/1706bc74-4783-4026-85fa-cbdb4082da32)

But there is still one case when no data is loss, it's when the edited file is at that last position in the data part. Entries table is just shifted and the old file at last position is overridden.

![rect6730](https://github.com/user-attachments/assets/4590921a-4512-436a-b688-5bf9c2025d52)

But if we decide to edit a new file and save modifications, then "c" is no more at the last position.

![rect6732](https://github.com/user-attachments/assets/bcd84134-4d2b-4383-b11b-b41c39f326a3)

So, after a third edit and save of the "c" file, we have lost space again.

![rect6734](https://github.com/user-attachments/assets/09e223a5-f1fc-4403-9762-25dec57b9e6e)

To recover lost space, you just have to launch a "defrag" operation.

![rect5452](https://github.com/user-attachments/assets/fb5f3c7a-2c44-47c3-ad0e-b8a545cfa17c)

In BGA, the "defrag" can be executed from the "File" menu, it is like a save operation but all lost space is recovered. Note that a "Save As" operation automatically execute a "defrag".
