# White Hole
White Hole is a self published sci-fi novel. The R script analyses the text in the novel for word frequency and relationships. It is a novice text mining/ data science project.

# Usage
Due to copyright, the content of the novel, i.e. the text, cannot be made public. The underlying word document which has been used in the code, has therefore *not* been uploaded. No such document will be uploaded in the future. 

The code can be used with any other text document. 

Line 12 of the R script needs to modified as follows:

```
unnamed<-read_docx(file="path/file-name.docx")
```
For documents in other formats (eg .txt) the function **read_docx** can be replaced with its file type compatible counterparts (eg **read_txt**). Visit [R Documentation](https://www.rdocumentation.org/) for more help.

Please note that some of the code is White Hole specific, and needs to be modified slightly to suit other documents. The required changes have been mentioned in the comments.

# Authors
[Kasturi Mitra](https://github.com/kasturimitra)
