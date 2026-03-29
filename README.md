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
  id: int,
  author: text,
  "timestamp": timestamp,
  msg: text,
  comments: [
    {
      "timestamp": timestamp,
      comment: text,
    }
  ],
}

fn FindPosts(id: int, madeBy: int, since: date, until: date) [ForumPost]! {
  var tempDate: date = null;

  if until < since {
    tempDate = since;
    since = until;
    until = tempDate;
  }

  var mut forumPosts: [ForumPost]! = [];

  for rec post in posts
                  where posts.id = id
                    and author = madeBy
                    and "date" >= since
                    and "date" <= until
                  order by "date" desc, "time" desc {
    var forumPost: ForumPost = {
      id: post.id,
      author: post.author,
      "timestamp": timestamp(post.date, post.time),
      msg: post.message,
      comments: [],
    };

    for rec comment in comments
                       where comments.id = post.id
                       order by "timestamp" {
      forumPost.comments += {
        comment: comment.text,
        "timestamp": timestamp(comment.date, comment.time),
      };
    }

    forumPosts += forumPost;
  }

  return forumPosts;
}
~~~
