Tablo: A Procedural Language for Relational Databases
=====================================================

Tablo is a procedural language designed to executed on relational database data.

> [!CAUTION]
> This project is nowhere near finished. It's not even in a state where it can be run. Almost any aspect of this project's design could change at any time.

The primary purpose of this language is to provide a syntax for generating ORM code in a type-safe manner. When code is to be generated, the compiled would read the database schema (in the form of a database DDL file) and generate code in the required output formats. However, I am also considering the possibility of supporting the ability to execute Tablo in the database as an alternative to languages such as PL/pgSQL.

At present, Tablo syntax looks like this:

~~~
with exampledb;

obj ForumPost {
  id: posts.id,
  author: users.surname + ', ' + users."given-names",
  "timestamp": str(posts.date) + ' ' + str(posts),
  msg: posts.text,
  comments: {
    "timestamp": str(comments.date) + ' ' + str(comments.time),
    comment: comments.text,
  }[],
}

fn FindPosts(id?: i32, madeBy: i32, since?: date, until?: date): ForumPost[] {
  return find each ForumPost from posts
    left join users on users.id = posts.author
    left join comments on comments.post = posts.id
    where posts.id = id
      and posts.author = madeBy
      and posts.date >= since
      and posts.date <= until
    order by posts.date desc, posts.time desc;
}
~~~