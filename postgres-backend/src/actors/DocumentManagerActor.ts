/**
 * Document Manager Actor - Gen_Server style with PostgreSQL
 *
 * Manages WOO documents with:
 * - PostgreSQL transactions for consistency
 * - LISTEN/NOTIFY for real-time updates
 * - Full-text search
 * - Automatic audit trail
 */

import { Pool } from 'pg';
import { Message, MessageHandler } from './ActorSystem.js';

export interface WOORequest {
  id: string;
  document_id: string;
  title: string;
  status: string;
  submitted_date: string;
  decided_date?: string;
  organization: string;
  organization_type: string;
  category: string;
  subject: string;
  requester: string;
  handler: string;
  metadata?: Record<string, any>;
  created_at: string;
  updated_at: string;
}

export interface DocumentManagerState {
  pool: Pool;
  cache: Map<string, WOORequest>;
  subscribers: Set<(event: any) => void>;
}

/**
 * Create Document Manager behavior
 */
export function createDocumentManagerBehavior(pool: Pool): MessageHandler {
  return async (message: Message, state: DocumentManagerState): Promise<DocumentManagerState> => {
    // Initialize state on first message
    if (!state.pool) {
      state = {
        pool,
        cache: new Map(),
        subscribers: new Set()
      };
    }

    switch (message.type) {
      case 'get_all': {
        const result = await pool.query(`
          SELECT * FROM woo_requests_view
          ORDER BY updated_at DESC
        `);

        // Update cache
        result.rows.forEach(row => {
          state.cache.set(row.document_id, row);
        });

        // Notify subscribers
        state.subscribers.forEach(sub => sub({
          type: 'documents_loaded',
          documents: result.rows
        }));

        return state;
      }

      case 'get_by_id': {
        const { document_id } = message.data;

        // Check cache first
        if (state.cache.has(document_id)) {
          return state;
        }

        const result = await pool.query(`
          SELECT * FROM woo_requests_view
          WHERE document_id = $1
        `, [document_id]);

        if (result.rows.length > 0) {
          state.cache.set(document_id, result.rows[0]);
        }

        return state;
      }

      case 'get_by_organization': {
        const { organization } = message.data;

        const result = await pool.query(`
          SELECT * FROM woo_requests_view
          WHERE organization = $1
          ORDER BY updated_at DESC
        `, [organization]);

        return state;
      }

      case 'get_by_status': {
        const { status } = message.data;

        const result = await pool.query(`
          SELECT * FROM woo_requests_view
          WHERE status = $1
          ORDER BY updated_at DESC
        `, [status]);

        return state;
      }

      case 'update_status': {
        const { document_id, new_status } = message.data;

        // Update in PostgreSQL (triggers will handle history and NOTIFY)
        const result = await pool.query(`
          UPDATE woo_requests
          SET status = $2
          WHERE document_id = $1
          RETURNING *
        `, [document_id, new_status]);

        if (result.rows.length > 0) {
          // Update cache
          const doc = result.rows[0];
          state.cache.set(document_id, doc);

          console.log(`[DocumentManager] Updated ${document_id}: ${new_status}`);
        }

        return state;
      }

      case 'insert': {
        const { document } = message.data;

        const result = await pool.query(`
          INSERT INTO woo_requests (
            document_id, title, status, submitted_date,
            organization_id, category, subject, requester, handler, metadata
          ) VALUES (
            $1, $2, $3, $4,
            (SELECT id FROM organizations WHERE name = $5),
            $6, $7, $8, $9, $10
          )
          RETURNING *
        `, [
          document.document_id,
          document.title,
          document.status,
          document.submitted_date,
          document.organization,
          document.category,
          document.subject,
          document.requester,
          document.handler,
          JSON.stringify(document.metadata || {})
        ]);

        if (result.rows.length > 0) {
          state.cache.set(document.document_id, result.rows[0]);
          console.log(`[DocumentManager] Inserted ${document.document_id}`);
        }

        return state;
      }

      case 'search': {
        const { query } = message.data;

        // Use full-text search function
        const result = await pool.query(`
          SELECT * FROM search_documents($1)
        `, [query]);

        console.log(`[DocumentManager] Search "${query}": ${result.rows.length} results`);

        return state;
      }

      case 'get_statistics': {
        // Query materialized view for fast stats
        const result = await pool.query(`
          SELECT * FROM woo_statistics
        `);

        return state;
      }

      case 'refresh_statistics': {
        // Refresh materialized view
        await pool.query(`SELECT refresh_statistics()`);
        console.log('[DocumentManager] Statistics refreshed');

        return state;
      }

      case 'subscribe': {
        const { callback } = message.data;
        state.subscribers.add(callback);
        console.log(`[DocumentManager] Subscriber added (total: ${state.subscribers.size})`);

        return state;
      }

      case 'unsubscribe': {
        const { callback } = message.data;
        state.subscribers.delete(callback);
        console.log(`[DocumentManager] Subscriber removed (total: ${state.subscribers.size})`);

        return state;
      }

      case 'postgres_notify': {
        // Received from LISTEN/NOTIFY
        const { channel, payload } = message.data;

        console.log(`[DocumentManager] Received NOTIFY on ${channel}:`, payload);

        // Invalidate cache for changed document
        if (payload.document_id) {
          state.cache.delete(payload.document_id);
        }

        // Notify all subscribers
        state.subscribers.forEach(sub => sub({
          type: channel,
          data: payload
        }));

        return state;
      }

      case 'clear_cache': {
        const oldSize = state.cache.size;
        state.cache.clear();
        console.log(`[DocumentManager] Cache cleared (${oldSize} entries)`);

        return state;
      }

      default:
        console.warn(`[DocumentManager] Unknown message type: ${message.type}`);
        return state;
    }
  };
}

/**
 * Simulation Actor - Automatically progress document statuses
 */
export function createSimulationBehavior(pool: Pool, documentManagerPid: string): MessageHandler {
  const workflow = [
    'Ontvangen',
    'In behandeling',
    '1e Concept',
    '2e Concept',
    'Definitief',
    'Gepubliceerd'
  ];

  return async (message: Message, state: any): Promise<any> => {
    if (message.type === 'tick') {
      // Get all documents not yet published
      const result = await pool.query(`
        SELECT document_id, status
        FROM woo_requests
        WHERE status != 'Gepubliceerd'
        ORDER BY RANDOM()
        LIMIT 1
      `);

      if (result.rows.length > 0) {
        const doc = result.rows[0];
        const currentIndex = workflow.indexOf(doc.status);

        if (currentIndex >= 0 && currentIndex < workflow.length - 1) {
          const nextStatus = workflow[currentIndex + 1];

          // Update status (will trigger NOTIFY)
          await pool.query(`
            UPDATE woo_requests
            SET status = $2
            WHERE document_id = $1
          `, [doc.document_id, nextStatus]);

          console.log(`[Simulation] ${doc.document_id}: ${doc.status} → ${nextStatus}`);
        }
      } else {
        // All published, reset one random document
        const resetResult = await pool.query(`
          SELECT document_id
          FROM woo_requests
          WHERE status = 'Gepubliceerd'
          ORDER BY RANDOM()
          LIMIT 1
        `);

        if (resetResult.rows.length > 0) {
          await pool.query(`
            UPDATE woo_requests
            SET status = 'Ontvangen'
            WHERE document_id = $1
          `, [resetResult.rows[0].document_id]);

          console.log(`[Simulation] Reset ${resetResult.rows[0].document_id} to Ontvangen`);
        }
      }
    }

    return state;
  };
}
