#!/bin/bash

# WOO Dashboard + DMS System Verification Script
# This script checks if all components are running and accessible

echo "🔍 WOO Dashboard + DMS System Verification"
echo "==========================================="
echo ""

# Color codes
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Counter for passed/failed checks
PASSED=0
FAILED=0

# Function to check HTTP endpoint
check_http() {
    local name=$1
    local url=$2
    local expected=${3:-200}

    echo -n "Checking $name... "

    response=$(curl -s -o /dev/null -w "%{http_code}" "$url" 2>/dev/null)

    if [ "$response" = "$expected" ] || [ "$response" = "200" ]; then
        echo -e "${GREEN}✓ OK${NC} (HTTP $response)"
        ((PASSED++))
        return 0
    else
        echo -e "${RED}✗ FAILED${NC} (HTTP $response, expected $expected)"
        ((FAILED++))
        return 1
    fi
}

# Function to check Docker container
check_docker() {
    local name=$1
    local container=$2

    echo -n "Checking Docker: $name... "

    if docker ps --format '{{.Names}}' | grep -q "$container"; then
        echo -e "${GREEN}✓ Running${NC}"
        ((PASSED++))
        return 0
    else
        echo -e "${RED}✗ Not Running${NC}"
        ((FAILED++))
        return 1
    fi
}

# Function to check TCP port
check_port() {
    local name=$1
    local port=$2

    echo -n "Checking port $port ($name)... "

    if nc -z localhost "$port" 2>/dev/null; then
        echo -e "${GREEN}✓ Open${NC}"
        ((PASSED++))
        return 0
    else
        echo -e "${RED}✗ Closed${NC}"
        ((FAILED++))
        return 1
    fi
}

echo "📊 Core Services"
echo "----------------"
check_http "WOO Dashboard" "http://localhost:5173" "200"
check_http "DMS Simulator API" "http://localhost:3001/health" "200"
check_http "NATS Event Consumer" "http://localhost:3002/health" "200"

echo ""
echo "🗄️  NATS JetStream"
echo "------------------"
check_docker "NATS Server" "nats-events-nats"
check_docker "Event Consumer" "nats-events-event-consumer"
check_http "NATS Monitoring" "http://localhost:8222/varz" "200"
check_port "NATS Client Port" "4222"

echo ""
echo "📄 Document Management Systems"
echo "------------------------------"

# Check Paperless-ngx
echo "Paperless-ngx:"
if docker ps --format '{{.Names}}' | grep -q "paperless-ngx-dms"; then
    check_docker "  Database" "paperless-ngx-dms-db"
    check_docker "  Redis" "paperless-ngx-dms-redis"
    check_docker "  MinIO" "paperless-ngx-dms-minio"
    check_docker "  Webserver" "paperless-ngx-dms-webserver"
    check_http "  API" "http://localhost:8000/api/" "200"
    check_http "  MinIO Health" "http://localhost:9000/minio/health/live" "200"
    check_port "  PostgreSQL" "5433"
else
    echo -e "${YELLOW}  Paperless-ngx not running (optional)${NC}"
fi

echo ""

# Check Alfresco
echo "Alfresco:"
if docker ps --format '{{.Names}}' | grep -q "alfresco-dms"; then
    check_docker "  Database" "alfresco-dms-postgres"
    check_docker "  MinIO" "alfresco-dms-minio"
    check_docker "  Repository" "alfresco-dms-alfresco"
    check_docker "  Share" "alfresco-dms-share"
    check_http "  Repository Health" "http://localhost:8080/alfresco" "200"
    check_http "  MinIO Health" "http://localhost:9002/minio/health/live" "200"
    check_port "  PostgreSQL" "5434"
else
    echo -e "${YELLOW}  Alfresco not running (optional)${NC}"
fi

echo ""
echo "🔌 API Integration Tests"
echo "------------------------"

# Test DMS Simulator connections
echo -n "DMS Simulator connections... "
connections=$(curl -s http://localhost:3001/test-connections 2>/dev/null)
if [ $? -eq 0 ]; then
    echo -e "${GREEN}✓ OK${NC}"
    echo "  $connections"
    ((PASSED++))
else
    echo -e "${RED}✗ FAILED${NC}"
    ((FAILED++))
fi

# Test NATS events endpoint
echo -n "NATS events endpoint... "
events=$(curl -s http://localhost:3002/events?limit=1 2>/dev/null)
if [ $? -eq 0 ]; then
    echo -e "${GREEN}✓ OK${NC}"
    ((PASSED++))
else
    echo -e "${RED}✗ FAILED${NC}"
    ((FAILED++))
fi

# Test NATS stats endpoint
echo -n "NATS stats endpoint... "
stats=$(curl -s http://localhost:3002/stats 2>/dev/null)
if [ $? -eq 0 ]; then
    echo -e "${GREEN}✓ OK${NC}"
    echo "  $stats"
    ((PASSED++))
else
    echo -e "${RED}✗ FAILED${NC}"
    ((FAILED++))
fi

echo ""
echo "📊 Summary"
echo "----------"
echo -e "Passed: ${GREEN}$PASSED${NC}"
echo -e "Failed: ${RED}$FAILED${NC}"
echo ""

if [ $FAILED -eq 0 ]; then
    echo -e "${GREEN}✓ All checks passed!${NC}"
    echo ""
    echo "🎉 System is ready to use!"
    echo ""
    echo "Next steps:"
    echo "  1. Open dashboard: http://localhost:5173"
    echo "  2. Try the DMS Simulator to upload test documents"
    echo "  3. View events in the Event Stream Viewer"
    echo ""
    exit 0
else
    echo -e "${RED}✗ Some checks failed${NC}"
    echo ""
    echo "Troubleshooting:"
    echo "  1. Check if all Docker services are running:"
    echo "     docker-compose -f nats-events/docker-compose.yml ps"
    echo "     docker-compose -f paperless-ngx-dms/docker-compose.yml ps"
    echo ""
    echo "  2. Check logs for errors:"
    echo "     docker-compose -f nats-events/docker-compose.yml logs"
    echo ""
    echo "  3. See COMPLETE_SYSTEM_GUIDE.md for detailed setup"
    echo ""
    exit 1
fi
