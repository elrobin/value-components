/** 
Data collection for paper - A value creation model from science-society interactions

Authors: I Ramos-Vielba, N Robinson-Garcia and R Woolley

Dataset includes publications of a set of researchers affiliated to Spanish institutions in the 2013-2015 period and their associated altmetric
mentions and OA status
**/
-- Select publications for the 2013-2015 period in altmetric.com
drop table #altmetric1315
select a.*
into #altmetric1315
from userdb_robinsonn..extra_altmetric_indicators_ut as a
join userdb_robinsonn..extra_ut_pub_details as b on a.ut=b.ut and a.id_extra=b.id_extra
where (pub_year between 2013 and 2015) and (doc_type='Article' or doc_type='Review')
--90232
--Count number of publications for articles and reviews in the 2013-2015 period
drop table #pubs
select distinct a.id_extra, count(distinct a.ut) as p, count(distinct b.doi) as p_altmetric
into #pubs
from userdb_robinsonn..extra_ut_pub_details as a
left join userdb_robinsonn..extra_altmetric_indicators_ut as b on a.ut=b.ut and a.id_extra=b.id_extra
where (pub_year between 2013 and 2015) and (doc_type='Article' or doc_type='Review')
group by a.id_extra
--11331
-- Twitter
drop table #twitter
select distinct id_extra, count(distinct doi) as p_twitter
into #twitter
from #altmetric1315
where twitter_unique_users_count is not null
group by id_extra
--7129

-- News
drop table #news
select distinct id_extra, count(distinct doi) as p_news
into #news
from #altmetric1315
where news_unique_users_count is not null
group by id_extra
--1772

-- Policy
drop table #policy
select distinct id_extra, count(distinct doi) as p_policy
into #policy
from #altmetric1315
where policy_unique_users_count is not null
group by id_extra
--387

-- OA
drop table #oa
select distinct a.id_extra, count(distinct a.doi) as OA
into #oa
from #altmetric1315 as a
join unpaywall_2021feb..pub as b on a.doi=b.doi
where b.is_oa=1
group by a.id_extra
--8374

select a.*, d.OA, f.p_twitter, b.p_news, e.p_policy
from #pubs as a
left join #news as b on a.id_extra=b.id_extra
left join #oa as d on a.id_extra=d.id_extra
left join #policy as e on a.id_extra=e.id_extra
left join #twitter as f on a.id_extra=f.id_extra
--

select distinct a.id_extra, a.ut, b.title
from #altmetric1315 as a
join userdb_robinsonn..extra_ut_pub_details as b on a.ut=b.ut

