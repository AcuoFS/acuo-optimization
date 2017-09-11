MATCH (a:Agreement)<-[:CLIENT_SIGNS]-(l1)<-[:MANAGES]-(:Client {id:'999'})
MATCH (leg:Leg)<-[:PAYS]-(t:Trade)-[:BELONGS_TO]->(p:Portfolio)<-[:VALUATED]-(v:MarginValuation)
WHERE (p)-[:FOLLOWS]->(a)
AND p.id in {portfolioIds}
AND a.type in ['bilateral','cleared']
AND (v.callType='Initial' OR (v.callType='Variation' AND a.type='bilateral'))

RETURN  distinct leg.currency as currency, filter(x IN labels(t) WHERE x <> 'Trade')[0] as tradeType,
t.id as tradeId, p.id as portfolioId, a.id as agreementId,a.type as agreementType
  
order by portfolioId
