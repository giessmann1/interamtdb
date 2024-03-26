db = new Mongo().getDB("interamtdb");
db.createCollection('jobads', { capped: false });