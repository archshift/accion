use std::fmt::{self, Formatter, Debug, Error};

pub struct SExp<'a, 'b> {
    f: &'b mut Formatter<'a>
}

impl<'a, 'b> SExp<'a, 'b> {
    pub fn new(f: &'b mut Formatter<'a>, name: &str) -> Result<Self, Error> {
        f.write_fmt(format_args!("({}", name))?;
        Ok(Self { f })
    }
    pub fn add(self, val: &dyn Debug) -> Result<Self, Error> {
        self.f.write_fmt(format_args!(" {:?}", val))?;
        Ok(self)
    }
    pub fn add_all<D>(mut self, it: impl Iterator<Item=D>) -> Result<Self, Error>
        where D: Debug
    {
        for item in it {
            self = self.add(&item)?;
        }
        Ok(self)
    }
    pub fn finish(self) -> fmt::Result {
        self.f.write_str(")")
    }
}