# Seating Chart App

## Description
This app uses machine learning algorithms to make seating charts for teachers. The app allows you to upload a file in the format shown below and returns a seating chart to separate groups.

## Data Structure

The following structure should be used to format incoming data.

**Name** is the name of the student.

**Group** can be anything. It's treated as a factor, so as long as the groups are kept consistent and case sensitive you'll be fine. Students don't need to be in a group.

**FrontRow** is a true or false variable indicating that a student should sit in the front row, regardless of group. This is calculated before grouping, so it should still be fine.

### Example data

| Name | Group | FrontRow |
| --- | --- | ---|
| Fred | 1 | FALSE |
| Wilma | NA | TRUE |