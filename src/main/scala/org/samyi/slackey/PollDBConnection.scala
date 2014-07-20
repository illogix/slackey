package org.samyi.slackey

import com.mongodb.casbah.Imports._
import com.mongodb.casbah.MongoClientURI

class PollDBConnection(uri: String, dbname: String) {
    val mc = MongoClient(MongoClientURI(uri))
    val db = mc(dbname)

    def wipe(col: MongoCollection) = {
        col.remove(MongoDBObject())
    }

    // pollseq
    val pollseqdb = db("pollseq")
    def nextPollId: Int = {
        val ret = pollseqdb.findAndModify[MongoDBObject, MongoDBObject, MongoDBObject, MongoDBObject](
            MongoDBObject("_id" -> "poll"),
            MongoDBObject(), MongoDBObject(), remove = false,
            $inc("seq" -> 1), returnNew = true, upsert = true)
        Int.unbox(ret.seq.head.get("seq"))
    }

    // poll
    val polldb = db("poll")

    def dboToPoll(dbo: DBObject): Poll = {
        val id = dbo.as[Int]("_id")
        val question = dbo.as[String]("question")
        val choices = dbo.as[MongoDBList]("choices").toList collect { case s: String => s }
        val anon = dbo.as[Boolean]("anon")
        val ptype = dbo.as[String]("type")
        val start = dbo.as[Long]("start")
        val timeout = dbo.as[Int]("timeout")
        val expired = dbo.as[Boolean]("expired")
        val author = dbo.as[String]("author")
        val channel = dbo.as[String]("channel")

        Poll(id, question, choices, anon, ptype, start, timeout, expired, author, channel)
    }

    def getPoll(pollId: Int) = {
        val query = MongoDBObject("_id" -> pollId)
        polldb.findOne(query) match {
            case Some(dbo) => Some(dboToPoll(dbo))
            case None => None
        }
    }

    def getPolls: Iterator[Poll] = {
        val query = MongoDBObject()
        for (dbo <- polldb.find(query)) yield dboToPoll(dbo)
    }

    def getActivePolls: Iterator[Poll] = {
        val query = MongoDBObject("expired" -> false)
        for (dbo <- polldb.find(query)) yield dboToPoll(dbo)
    }

    def expirePoll(pollId: Int) = {
        val query = MongoDBObject("_id" -> pollId)
        val upd = $set("expired" -> true)
        votedb.update(query, upd)
    }

    def addPoll(poll: Poll) = {
        val pollId = nextPollId
        val query = MongoDBObject(
            "_id" -> pollId,
            "question" -> poll.question,
            "choices" -> poll.choices,
            "anon" -> poll.anon,
            "type" -> poll.ptype,
            "start" -> poll.start,
            "timeout" -> poll.timeout,
            "expired" -> false,
            "author" -> poll.author,
            "channel" -> poll.channel
        )
        polldb.insert(query)
        poll.copy(id = pollId)
    }

    // vote
    val votedb = db("vote")

    def dboToVote(dbo: DBObject): Vote = {
        val pollId = dbo.as[Int]("poll_id")
        val voter = dbo.as[String]("voter")
        val choice = dbo.as[String]("choice")
        val time = dbo.as[Long]("time")

        Vote(pollId, voter, choice, time)
    }

    def getResults(poll: Poll): Map[String, List[Vote]] = {
        val query = MongoDBObject("poll_id" -> poll.id)
        val votes = for (dbo <- votedb.find(query)) yield dboToVote(dbo)
        val voteMap = votes.toList.groupBy(_.choice)

        poll.choices.map(c => c -> voteMap.getOrElse(c, List())).toMap
    }

    def vote(vote: Vote) = {
        val query = MongoDBObject(
            "poll_id" -> vote.pollId,
            "voter" -> vote.voter
        )
        val upd = $set("choice" -> vote.choice, "time" -> vote.time)
        votedb.update(query, upd, upsert = true)
    }
}
