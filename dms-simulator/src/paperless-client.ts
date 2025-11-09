import axios from 'axios';
import FormData from 'form-data';
import { DocumentMetadata } from './document-generator.js';

export class PaperlessClient {
  private baseUrl: string;
  private apiKey: string;

  constructor(baseUrl: string, apiKey: string) {
    this.baseUrl = baseUrl;
    this.apiKey = apiKey;
  }

  async uploadDocument(
    pdfBuffer: Buffer,
    metadata: DocumentMetadata
  ): Promise<{ id: number; success: boolean }> {
    const form = new FormData();

    // Create filename
    const filename = `${metadata.date.toISOString().split('T')[0]}_${metadata.type}_${metadata.category.replace(/\s+/g, '_')}.pdf`;

    form.append('document', pdfBuffer, {
      filename,
      contentType: 'application/pdf',
    });

    form.append('title', metadata.title);
    form.append('created', metadata.date.toISOString().split('T')[0]);

    // Tags (create if not exists)
    const tagIds = await this.ensureTags(metadata.tags);
    tagIds.forEach(tagId => {
      form.append('tags', String(tagId));
    });

    try {
      const response = await axios.post(
        `${this.baseUrl}/api/documents/post_document/`,
        form,
        {
          headers: {
            ...form.getHeaders(),
            'Authorization': `Token ${this.apiKey}`,
          },
          maxBodyLength: Infinity,
          maxContentLength: Infinity,
        }
      );

      return {
        id: response.data.id || response.data,
        success: true,
      };
    } catch (error: any) {
      console.error('Paperless upload error:', error.response?.data || error.message);
      throw error;
    }
  }

  private async ensureTags(tagNames: string[]): Promise<number[]> {
    const tagIds: number[] = [];

    for (const tagName of tagNames) {
      try {
        // Try to get existing tag
        const response = await axios.get(
          `${this.baseUrl}/api/tags/`,
          {
            params: { name: tagName },
            headers: { 'Authorization': `Token ${this.apiKey}` },
          }
        );

        if (response.data.results && response.data.results.length > 0) {
          tagIds.push(response.data.results[0].id);
        } else {
          // Create new tag
          const createResponse = await axios.post(
            `${this.baseUrl}/api/tags/`,
            { name: tagName },
            { headers: { 'Authorization': `Token ${this.apiKey}` } }
          );
          tagIds.push(createResponse.data.id);
        }
      } catch (error) {
        console.warn(`Failed to create/get tag "${tagName}":`, error);
      }
    }

    return tagIds;
  }

  async testConnection(): Promise<boolean> {
    try {
      const response = await axios.get(`${this.baseUrl}/api/`, {
        headers: { 'Authorization': `Token ${this.apiKey}` },
      });
      return response.status === 200;
    } catch (error) {
      return false;
    }
  }
}
