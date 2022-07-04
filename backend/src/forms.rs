use crate::entry_type::EntryType;
use crate::date::Date;

use serde::{Serialize, Deserialize};

#[derive(Deserialize, Debug)]
pub struct NewEntryForm 
{
    pub link : String,
    pub title : String,
    pub description : String,
    pub author : String,
    pub category : String,
    pub themes : Vec<String>,
    pub works_mentioned : Vec<String>,
    pub tags : Vec<String>,
    pub date_published : Date,
    pub exceptional : bool,
    pub entry_type : EntryType
}

#[derive(Serialize, Deserialize, Debug, Clone, Default)]
pub struct Entry 
{
    pub id : i64,
    pub link : String,
    pub title : String,
    pub description : String,
    pub author : String,
    pub category : String,
    pub themes : Vec<String>,
    pub works_mentioned : Vec<String>,
    pub tags : Vec<String>,
    pub date_published : Date,
    pub date_saved : Date,
    pub exceptional : bool,
    pub entry_type : EntryType,
    pub image : Option<String>,
    pub backup : Option<String>
}

#[derive(Deserialize)]
pub struct ImageLinkForm 
{
    pub image_url : String
}

#[derive(Deserialize)]
pub struct BackupLinkForm 
{
    pub backup_url : String
}